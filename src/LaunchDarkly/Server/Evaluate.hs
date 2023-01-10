{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

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
import qualified Data.HashSet as                     HS
import qualified Data.Vector as                      V
import qualified Data.Text as                        T
import qualified Data.ByteString as                  B
import qualified Data.ByteString.Base16 as           B16
import           Data.Text.Encoding                  (encodeUtf8)
import           GHC.Natural                         (Natural)
import           Data.Word                           (Word8)
import           Data.ByteString                     (ByteString)
import           Data.HashSet (HashSet)

import           LaunchDarkly.Server.Context         (Context(..), getKey, getValue, getKinds, getIndividualContext)
import           LaunchDarkly.Server.Client.Internal (ClientI, Status(Initialized), getStatusI)
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
    else evaluateInternalClient client key context (wrap fallback) includeReason >>= \detail -> pure $ maybe
        (EvaluationDetail fallback Nothing $ if isError (getField @"reason" detail) then (getField @"reason" detail) else EvaluationReasonError EvalErrorWrongType)
        (setValue detail) (convert $ getField @"value" detail)

evaluateInternalClient :: ClientI -> Text -> Context -> Value -> Bool -> IO (EvaluationDetail Value)
evaluateInternalClient _ _ (Invalid _) fallback _ = pure $ errorDefault EvalErrorInvalidContext fallback
evaluateInternalClient client key context fallback includeReason = do
    (detail, unknown, events) <- getFlagC (getField @"store" client) key >>= \case
        Left err          -> do
            let event = newUnknownFlagEvent key fallback (EvaluationReasonError $ EvalErrorExternalStore err)
            pure (errorDetail $ EvalErrorExternalStore err, True, pure event)
        Right Nothing     -> do
            let event = newUnknownFlagEvent key fallback (EvaluationReasonError EvalErrorFlagNotFound)
            pure (errorDefault EvalErrorFlagNotFound fallback, True, pure event)
        Right (Just flag) -> do
            (detail, events) <- evaluateDetail flag context HS.empty $ getField @"store" client
            let detail' = setFallback detail fallback
            pure (detail', False, flip (:) events $ newSuccessfulEvalEvent flag (getField @"variationIndex" detail')
                (getField @"value" detail') (Just fallback) (getField @"reason" detail') Nothing)
    processEvalEvents (getField @"config" client) (getField @"events" client) context includeReason events unknown
    pure detail

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

evaluateDetail :: (Monad m, LaunchDarklyStoreRead store m) => Flag -> Context -> HS.HashSet Text -> store -> m (EvaluationDetail Value, [EvalEvent])
evaluateDetail flag@(getField @"on" -> False) _ _ _ = pure (getOffValue flag EvaluationReasonOff, [])
evaluateDetail flag context seenFlags store
    | HS.member (getField @"key" flag) seenFlags = pure (getOffValue flag $ EvaluationReasonError EvalErrorKindMalformedFlag, [])
    | otherwise = checkPrerequisites flag context (HS.insert (getField @"key" flag) seenFlags) store >>= \case
        (Nothing, events)     -> evaluateInternal flag context store >>= (\x -> pure (x, events))
        (Just detail, events) -> pure (detail, events)

status :: Prerequisite -> EvaluationDetail a -> Flag -> Bool
status prereq result prereqFlag = getField @"on" prereqFlag && (getField @"variationIndex" result) ==
    pure (getField @"variation" prereq)

sequenceUntil :: Monad m => (a -> Bool) -> [m a] -> m [a]
sequenceUntil _ []     = return []
sequenceUntil p (m:ms) = m >>= \a -> if p a then return [a] else
    sequenceUntil p ms >>= \as -> return (a:as)

checkPrerequisites :: (Monad m, LaunchDarklyStoreRead store m) => Flag -> Context -> HS.HashSet Text -> store -> m (Maybe (EvaluationDetail Value), [EvalEvent])
checkPrerequisites flag context seenFlags store =
    let p = getField @"prerequisites" flag
    in if null p then pure (Nothing, [])
       else do
            evals <- sequenceUntil (isJust . fst) $ map (checkPrerequisite store context flag seenFlags) p
            pure (msum $ map fst evals, concatMap snd evals)

checkPrerequisite :: (Monad m, LaunchDarklyStoreRead store m) => store -> Context -> Flag -> HS.HashSet Text -> Prerequisite -> m (Maybe (EvaluationDetail Value), [EvalEvent])
checkPrerequisite store context flag seenFlags prereq =
    if HS.member (getField @"key" prereq) seenFlags then pure (Just $ errorDetail EvalErrorKindMalformedFlag, [])
    else getFlagC store (getField @"key" prereq) >>= \case
        Left err                -> pure (pure $ getOffValue flag $ EvaluationReasonError $ EvalErrorExternalStore err, [])
        Right Nothing           -> pure (pure $ getOffValue flag $ EvaluationReasonPrerequisiteFailed (getField @"key" prereq), [])
        Right (Just prereqFlag) -> evaluateDetail prereqFlag context seenFlags store >>= (process prereqFlag)
        where process prereqFlag (detail, events)
                | isError (getField @"reason" detail) = pure (Just $ errorDetail EvalErrorKindMalformedFlag, mempty)
                | otherwise = let event = newSuccessfulEvalEvent prereqFlag (getField @"variationIndex" detail) (getField @"value" detail) Nothing (getField @"reason" detail) (Just $ getField @"key" prereqFlag)
                            in if status prereq detail prereqFlag then pure (Nothing, event : events)
                                else pure (pure $ getOffValue flag $ EvaluationReasonPrerequisiteFailed (getField @"key" prereq), event : events)

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
        isRolloutExperiment = (getField @"kind" rollout) == RolloutKindExperiment
        bucketBy = fromMaybe "key" $ if isRolloutExperiment then Nothing else getField @"bucketBy" rollout
        variations = getField @"variations" rollout
        bucket = bucketContext context (getField @"contextKind" rollout) key bucketBy salt (getField @"seed" rollout)
        isExperiment = isRolloutExperiment && (isJust bucket)
        c acc i = acc >>= \acc -> let t = acc + ((getField @"weight" i) / 100000.0) in
            case bucket of
                Just v | v >= t -> Right t
                _ -> Left (Just $ getField @"variation" i, (not $ getField @"untracked" i) && isExperiment)
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

bucketContext :: Context -> Text -> Text -> Text -> Text -> Maybe Int -> Maybe Float
bucketContext context kind key attribute salt seed =
    case getIndividualContext kind context of
        Nothing -> Nothing
        Just ctx -> let bucketableString = bucketableStringValue $ getValue attribute ctx
                    in Just $ calculateBucketValue bucketableString key salt seed

calculateBucketValue :: (Maybe Text) -> Text -> Text -> Maybe Int -> Float
calculateBucketValue Nothing  _ _ _ = 0
calculateBucketValue (Just text) key salt seed =
    let seed' = case seed of
            Nothing    -> T.concat [key, ".", salt, ".", text]
            Just seed' -> T.concat [T.pack $ show seed', ".", text]
        byteString = B.take 15 $ B16.encode $ hash $ encodeUtf8 $ seed'
    in ((fromIntegral $ fromJust $ hexStringToNumber byteString) :: Float) / 0xFFFFFFFFFFFFFFF

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
        let bucket = bucketContext context (getField @"rolloutContextKind" rule) key (fromMaybe "key" $ getField @"bucketBy" rule) salt Nothing
        in case bucket of
            Just v | v >= (weight / 100000.0) -> False
            _ -> True)

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
