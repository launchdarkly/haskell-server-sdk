{-# LANGUAGE NamedFieldPuns #-}

module LaunchDarkly.Server.Evaluate where

import           Control.Lens                        ((%~))
import           Control.Monad                       (mzero, msum)
import           Control.Monad.Extra                 (ifM, anyM, allM, firstJustM)
import           Crypto.Hash.SHA1                    (hash)
import           Data.Scientific                     (Scientific, floatingOrInteger)
import           Data.Either                         (fromLeft)
import           Data.Function                       ((&))
import           Data.Aeson.Types                    (Value(..))
import           Data.Maybe                          (fromJust, isJust, fromMaybe)
import           Data.Text                           (Text)
import           Data.Generics.Product               (getField, field)
import           Data.List                           (genericIndex, find)
import qualified Data.Vector as                      V
import qualified Data.Text as                        T
import qualified Data.ByteString as                  B
import qualified Data.ByteString.Base16 as           B16
import           Data.Text.Encoding                  (encodeUtf8)
import           GHC.Natural                         (Natural)
import           Data.Word                           (Word8)
import           Data.ByteString                     (ByteString)
import           Data.HashSet (HashSet)

import           LaunchDarkly.Server.Context         (Context(..), toLegacyUser, getKey, getValue, getKinds, getIndividualContext)
import           LaunchDarkly.Server.Client.Internal (ClientI, Status(Initialized), getStatusI)
import           LaunchDarkly.Server.User.Internal   (User(..), valueOf)
import           LaunchDarkly.Server.Features        (Flag, Segment(..), SegmentTarget(..), Prerequisite, SegmentRule, Clause, VariationOrRollout, Rule, RolloutKind(RolloutKindExperiment))
import           LaunchDarkly.Server.Store.Internal  (LaunchDarklyStoreRead, getFlagC, getSegmentC)
import           LaunchDarkly.Server.Operators       (Op(OpSegmentMatch), getOperation)
import           LaunchDarkly.Server.Events          (EvalEvent, newUnknownFlagEvent, newSuccessfulEvalEvent, processEvalEvents)
import           LaunchDarkly.Server.Details         (EvaluationDetail(..), EvaluationReason(..), EvalErrorKind(..))

setFallback :: EvaluationDetail Value -> Value -> EvaluationDetail Value
setFallback detail fallback = case getField @"variationIndex" detail of
    Nothing -> detail { value = fallback }; _ -> detail

setValue :: EvaluationDetail Value -> a -> EvaluationDetail a
setValue x v = x { value = v }

isError :: EvaluationReason -> Bool
isError reason = case reason of (EvaluationReasonError _) -> True; _ -> False

evaluateTyped :: ClientI -> Text -> Context -> a -> (a -> Value) -> Bool -> (Value -> Maybe a) -> IO (EvaluationDetail a)
evaluateTyped client key context fallback wrap includeReason convert = getStatusI client >>= \status -> if status /= Initialized
    then pure $ EvaluationDetail fallback Nothing $ EvaluationReasonError EvalErrorClientNotReady
    else evaluateInternalClient client key context (wrap fallback) includeReason >>= \r -> pure $ maybe
        (EvaluationDetail fallback Nothing $ if isError (getField @"reason" r)
            then (getField @"reason" r) else EvaluationReasonError EvalErrorWrongType)
        (setValue r) (convert $ getField @"value" r)

evaluateInternalClient :: ClientI -> Text -> Context -> Value -> Bool -> IO (EvaluationDetail Value)
evaluateInternalClient _ _ (Invalid _) fallback _ = pure $ errorDefault EvalErrorInvalidContext fallback
evaluateInternalClient client key context fallback includeReason = do
    (reason, unknown, events) <- getFlagC (getField @"store" client) key >>= \case
        Left err          -> do
            let event = newUnknownFlagEvent key fallback (EvaluationReasonError $ EvalErrorExternalStore err)
            pure (errorDetail $ EvalErrorExternalStore err, True, pure event)
        Right Nothing     -> do
            let event = newUnknownFlagEvent key fallback (EvaluationReasonError EvalErrorFlagNotFound)
            pure (errorDefault EvalErrorFlagNotFound fallback, True, pure event)
        Right (Just flag) -> do
            (reason, events) <- evaluateDetail flag context $ getField @"store" client
            let reason' = setFallback reason fallback
            pure (reason', False, flip (:) events $ newSuccessfulEvalEvent flag (getField @"variationIndex" reason')
                (getField @"value" reason') (Just fallback) (getField @"reason" reason') Nothing)
    processEvalEvents (getField @"config" client) (getField @"events" client) context includeReason events unknown
    pure reason

getOffValue :: Flag -> EvaluationReason -> EvaluationDetail Value
getOffValue flag reason = case getField @"offVariation" flag of
    Just offVariation -> getVariation flag offVariation reason
    Nothing -> EvaluationDetail { value = Null, variationIndex = mzero, reason = reason }

getVariation :: Flag -> Integer -> EvaluationReason -> EvaluationDetail Value
getVariation flag index reason
    | idx < 0 = EvaluationDetail { value = Null, variationIndex = mzero, reason = EvaluationReasonError EvalErrorKindMalformedFlag }
    | idx >= length variations = EvaluationDetail { value = Null, variationIndex = mzero, reason = EvaluationReasonError EvalErrorKindMalformedFlag }
    | otherwise = EvaluationDetail { value = genericIndex variations index, variationIndex = pure index, reason = reason }
  where idx = fromIntegral index
        variations = getField @"variations" flag

evaluateDetail :: (Monad m, LaunchDarklyStoreRead store m) => Flag -> Context -> store
    -> m (EvaluationDetail Value, [EvalEvent])
evaluateDetail flag context store = if getField @"on" flag
    then checkPrerequisites flag context store >>= \case
        (Nothing, events)     -> evaluateInternal flag context store >>= (\x -> pure (x, events))
        (Just reason, events) -> pure (getOffValue flag reason, events)
    else pure (getOffValue flag EvaluationReasonOff, [])

status :: Prerequisite -> EvaluationDetail a -> Flag -> Bool
status prereq result prereqFlag = getField @"on" prereqFlag && (getField @"variationIndex" result) ==
    pure (getField @"variation" prereq)

checkPrerequisite :: (Monad m, LaunchDarklyStoreRead store m) => store -> Context -> Flag -> Prerequisite
    -> m (Maybe EvaluationReason, [EvalEvent])
checkPrerequisite store context flag prereq = getFlagC store (getField @"key" prereq) >>= \case
    Left err                -> pure (pure $ EvaluationReasonError $ EvalErrorExternalStore err, [])
    Right Nothing           -> pure (pure $ EvaluationReasonPrerequisiteFailed (getField @"key" prereq), [])
    Right (Just prereqFlag) -> evaluateDetail prereqFlag context store >>= \(r, events) -> let
        event = newSuccessfulEvalEvent prereqFlag (getField @"variationIndex" r) (getField @"value" r) Nothing
            (getField @"reason" r) (Just $ getField @"key" flag)
        in if status prereq r prereqFlag then pure (Nothing, event : events) else
            pure (pure $ EvaluationReasonPrerequisiteFailed (getField @"key" prereq), event : events)

sequenceUntil :: Monad m => (a -> Bool) -> [m a] -> m [a]
sequenceUntil _ []     = return []
sequenceUntil p (m:ms) = m >>= \a -> if p a then return [a] else
    sequenceUntil p ms >>= \as -> return (a:as)

checkPrerequisites :: (Monad m, LaunchDarklyStoreRead store m) => Flag -> Context -> store
    -> m (Maybe EvaluationReason, [EvalEvent])
checkPrerequisites flag context store = let p = getField @"prerequisites" flag in if null p then pure (Nothing, []) else do
    evals <- sequenceUntil (isJust . fst) $ map (checkPrerequisite store context flag) p
    pure (msum $ map fst evals, concatMap snd evals)

evaluateInternal :: (Monad m, LaunchDarklyStoreRead store m) => Flag -> Context -> store -> m (EvaluationDetail Value)
evaluateInternal flag context store = result where
    checkTarget target = if elem (getKey context) (getField @"values" target)
        then Just $ getVariation flag (getField @"variation" target) EvaluationReasonTargetMatch else Nothing
    checkRule (ruleIndex, rule) = ifM (ruleMatchesContext rule context store)
        (pure $ Just $ getValueForVariationOrRollout flag (getField @"variationOrRollout" rule) context
            EvaluationReasonRuleMatch { ruleIndex = ruleIndex, ruleId = getField @"id" rule, inExperiment = False })
        (pure Nothing)
    fallthrough = getValueForVariationOrRollout flag (getField @"fallthrough" flag) context (EvaluationReasonFallthrough False)
    result = let
        ruleMatch   = checkRule <$> zip [0..] (getField @"rules" flag)
        targetMatch = return . checkTarget <$> getField @"targets" flag
        in fromMaybe fallthrough <$> firstJustM Prelude.id (ruleMatch ++ targetMatch)

errorDefault :: EvalErrorKind -> Value -> EvaluationDetail Value
errorDefault kind v = EvaluationDetail { value = v, variationIndex = mzero, reason = EvaluationReasonError kind }

errorDetail :: EvalErrorKind -> EvaluationDetail Value
errorDetail kind = errorDefault kind Null

getValueForVariationOrRollout :: Flag -> VariationOrRollout -> Context -> EvaluationReason -> EvaluationDetail Value
getValueForVariationOrRollout flag vr context reason =
    case variationIndexForContext vr context (getField @"key" flag) (getField @"salt" flag) of
        (Nothing, _)           -> errorDetail EvalErrorKindMalformedFlag
        (Just x, inExperiment) -> (getVariation flag x reason) & field @"reason" %~ setInExperiment inExperiment

setInExperiment :: Bool -> EvaluationReason -> EvaluationReason
setInExperiment inExperiment reason = case reason of
    EvaluationReasonFallthrough _         -> EvaluationReasonFallthrough inExperiment
    EvaluationReasonRuleMatch index idx _ -> EvaluationReasonRuleMatch index idx inExperiment
    x                                     -> x

ruleMatchesContext :: Monad m => LaunchDarklyStoreRead store m => Rule -> Context -> store -> m Bool
ruleMatchesContext rule context store =
    allM (\clause -> clauseMatchesContext store clause context) (getField @"clauses" rule)

variationIndexForContext :: VariationOrRollout -> Context -> Text -> Text -> (Maybe Integer, Bool)
variationIndexForContext vor context key salt
    | (Just variation) <- getField @"variation" vor = (pure variation, False)
    | (Just rollout) <- getField @"rollout" vor = let
        isExperiment = (getField @"kind" rollout) == RolloutKindExperiment
        variations = getField @"variations" rollout
        bucket = bucketContext context key (fromMaybe "key" $ getField @"bucketBy" rollout) salt (getField @"seed" rollout)
        c acc i = acc >>= \acc -> let t = acc + ((getField @"weight" i) / 100000.0) in
            if bucket < t then Left (Just $ getField @"variation" i, (not $ getField @"untracked" i) && isExperiment) else Right t
        in if null variations then (Nothing, False) else fromLeft
            (Just $ getField @"variation" $ last variations, (not $ getField @"untracked" $ last variations) && isExperiment) $
            foldl c (Right (0.0 :: Float)) variations
    | otherwise = (Nothing, False)

-- Bucketing -------------------------------------------------------------------

hexCharToNumber :: Word8 -> Maybe Natural
hexCharToNumber w = fmap fromIntegral $ if
    | 48 <= w && w <= 57  -> pure $ w - 48
    | 65 <= w && w <= 70  -> pure $ w - 55
    | 97 <= w && w <= 102 -> pure $ w - 87
    | otherwise           -> Nothing

hexStringToNumber :: ByteString -> Maybe Natural
hexStringToNumber bytes = B.foldl' step (Just 0) bytes where
    step acc x = acc >>= \acc' -> hexCharToNumber x >>= pure . (+) (acc' * 16)

bucketContext :: Context -> Text -> Text -> Text -> Maybe Int -> Float
bucketContext context key attribute salt seed = fromMaybe 0 $ do
    let (User user) = fromJust $ toLegacyUser context
    i <- valueOf user attribute >>= bucketableStringValue >>= \x -> pure $ B.take 15 $ B16.encode $ hash $ encodeUtf8 $
        case seed of
            Nothing    -> T.concat [key, ".", salt, ".", x]
            Just seed' -> T.concat [T.pack $ show seed', ".", x]
    pure $ ((fromIntegral $ fromJust $ hexStringToNumber i) :: Float) / (0xFFFFFFFFFFFFFFF)

floatingOrInteger' :: Scientific -> Either Double Integer
floatingOrInteger' = floatingOrInteger

bucketableStringValue :: Value -> Maybe Text
bucketableStringValue (String x) = pure x
bucketableStringValue (Number s) = either (const Nothing) (pure . T.pack . show) (floatingOrInteger' s)
bucketableStringValue _          = Nothing

-- Clause ----------------------------------------------------------------------

maybeNegate :: Clause -> Bool -> Bool
maybeNegate clause value = if getField @"negate" clause then not value else value

-- For a given clause, determine if the provided value matches that clause.
--
-- The operation to be check and the values to compare against are both extract from within the Clause itself.
matchAnyClauseValue :: Clause -> Value -> Bool
matchAnyClauseValue clause contextValue = any (f contextValue) v
  where f = getOperation $ getField @"op" clause
        v = getField @"values" clause

-- If attribute is "kind", then we treat operator and values as a match expression against a list of all individual
-- kinds in the context. That is, for a multi-kind context with kinds of "org" and "user", it is a match if either
-- of those strings is a match with Operator and Values.
clauseMatchesByKind :: Clause -> Context -> Bool
clauseMatchesByKind clause context  = foldr f False (getKinds context)
  where f kind result
          | result == True = True
          | otherwise = matchAnyClauseValue clause (String kind)

clauseMatchesContextNoSegments :: Clause -> Context -> Bool
clauseMatchesContextNoSegments clause context =
  if attr == "kind" then maybeNegate clause $ clauseMatchesByKind clause context
  else case getIndividualContext (getField @"contextKind" clause) context of
    Nothing -> False
    Just ctx -> case getValue attr ctx of
        Null    -> False
        Array a -> maybeNegate clause $ V.any (matchAnyClauseValue clause) a
        x       -> maybeNegate clause $ matchAnyClauseValue clause x
  where attr = getField @"attribute" clause

clauseMatchesContext :: (Monad m, LaunchDarklyStoreRead store m) => store -> Clause -> Context -> m Bool
clauseMatchesContext store clause context
    | getField @"op" clause == OpSegmentMatch = do
        let values = [ x | String x <- getField @"values" clause]
        x <- anyM (\k -> getSegmentC store k >>= pure . checkSegment) values
        pure $ maybeNegate clause x
    | otherwise = pure $ clauseMatchesContextNoSegments clause context
    where
        checkSegment :: Either Text (Maybe Segment) -> Bool
        checkSegment (Right (Just segment)) = segmentContainsContext segment context
        checkSegment _                      = False

-- Segment ---------------------------------------------------------------------

segmentRuleMatchesContext :: SegmentRule -> Context -> Text -> Text -> Bool
segmentRuleMatchesContext rule context key salt = (&&)
    (all (flip clauseMatchesContextNoSegments context) (getField @"clauses" rule))
    (flip (maybe True) (getField @"weight" rule) $ \weight ->
        bucketContext context key (fromMaybe "key" $ getField @"bucketBy" rule) salt Nothing < weight / 100000.0)

segmentContainsContext :: Segment -> Context -> Bool
segmentContainsContext (Segment { included, includedContexts, excluded, excludedContexts, key, salt, rules }) context
    | contextKeyInTargetList included "user" context = True
    | (any (flip contextKeyInSegmentTarget context) includedContexts) = True
    | contextKeyInTargetList excluded "user" context = False
    | (any (flip contextKeyInSegmentTarget context) excludedContexts) = False
    | Just _ <- find (\r -> segmentRuleMatchesContext r context key salt) rules = True
    | otherwise = False

contextKeyInSegmentTarget :: SegmentTarget -> Context -> Bool
contextKeyInSegmentTarget (SegmentTarget { values, contextKind }) = contextKeyInTargetList values contextKind

contextKeyInTargetList :: (HashSet Text) -> Text -> Context -> Bool
contextKeyInTargetList targets kind context = case getIndividualContext kind context of
    Just ctx -> elem (getKey ctx) targets
    Nothing -> False
