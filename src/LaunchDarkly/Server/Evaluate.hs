module LaunchDarkly.Server.Evaluate where

import           Control.Lens                        ((%~))
import           Control.Monad                       (mzero, msum)
import           Control.Monad.Extra                 (ifM, anyM, allM, firstJustM)
import           Crypto.Hash.SHA1                    (hash)
import           Data.Scientific                     (Scientific, floatingOrInteger)
import           Data.Either                         (either, fromLeft)
import           Data.Function                       ((&))
import           Data.Aeson.Types                    (Value(..))
import           Data.Maybe                          (maybe, fromJust, isJust, fromMaybe)
import           Data.Text                           (Text)
import           Data.Generics.Product               (getField, field)
import           Data.List                           (genericIndex, null, find)
import qualified Data.Vector as                      V
import qualified Data.Text as                        T
import qualified Data.ByteString as                  B
import qualified Data.ByteString.Base16 as           B16
import           Data.Text.Encoding                  (encodeUtf8)
import           GHC.Natural                         (Natural)
import           Data.Word                           (Word8)
import           Data.ByteString                     (ByteString)

import           LaunchDarkly.Server.Client.Internal (ClientI, Status(Initialized), getStatusI)
import           LaunchDarkly.Server.User.Internal   (UserI, valueOf)
import           LaunchDarkly.Server.Features        (Flag, Segment, Prerequisite, SegmentRule, Clause, VariationOrRollout, Rule, RolloutKind(RolloutKindExperiment))
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

evaluateTyped :: ClientI -> Text -> UserI -> a -> (a -> Value) -> Bool -> (Value -> Maybe a) -> IO (EvaluationDetail a)
evaluateTyped client key user fallback wrap includeReason convert = getStatusI client >>= \status -> if status /= Initialized
    then pure $ EvaluationDetail fallback Nothing $ EvaluationReasonError EvalErrorClientNotReady
    else evaluateInternalClient client key user (wrap fallback) includeReason >>= \r -> pure $ maybe
        (EvaluationDetail fallback Nothing $ if isError (getField @"reason" r)
            then (getField @"reason" r) else EvaluationReasonError EvalErrorWrongType)
        (setValue r) (convert $ getField @"value" r)

evaluateInternalClient :: ClientI -> Text -> UserI -> Value -> Bool -> IO (EvaluationDetail Value)
evaluateInternalClient client key user fallback includeReason = do
    (reason, unknown, events) <- getFlagC (getField @"store" client) key >>= \case
        Left err          -> do
            let event = newUnknownFlagEvent key fallback (EvaluationReasonError $ EvalErrorExternalStore err)
            pure (errorDetail $ EvalErrorExternalStore err, True, pure event)
        Right Nothing     -> do
            let event = newUnknownFlagEvent key fallback (EvaluationReasonError EvalErrorFlagNotFound)
            pure (errorDefault EvalErrorFlagNotFound fallback, True, pure event)
        Right (Just flag) -> do
            (reason, events) <- evaluateDetail flag user $ getField @"store" client
            let reason' = setFallback reason fallback
            pure (reason', False, flip (:) events $ newSuccessfulEvalEvent flag (getField @"variationIndex" reason')
                (getField @"value" reason') (Just fallback) (getField @"reason" reason') Nothing)
    processEvalEvents (getField @"config" client) (getField @"events" client) user includeReason events unknown
    pure reason

getOffValue :: Flag -> EvaluationReason -> EvaluationDetail Value
getOffValue flag reason = case getField @"offVariation" flag of
    Just offVariation
        | offVariation < 0 -> EvaluationDetail { value = Null, variationIndex = mzero, reason = reason }
        | otherwise -> getVariation flag offVariation reason
    Nothing -> EvaluationDetail { value = Null, variationIndex = mzero, reason = reason }

getVariation :: Flag -> Integer -> EvaluationReason -> EvaluationDetail Value
getVariation flag index reason = let variations = getField @"variations" flag in
    if fromIntegral index >= length variations
        then EvaluationDetail { value = Null, variationIndex = mzero, reason = EvaluationReasonError EvalErrorKindMalformedFlag }
        else EvaluationDetail { value = genericIndex variations index, variationIndex = pure index, reason = reason }

evaluateDetail :: (Monad m, LaunchDarklyStoreRead store m) => Flag -> UserI -> store
    -> m (EvaluationDetail Value, [EvalEvent])
evaluateDetail flag user store = if getField @"on" flag
    then checkPrerequisites flag user store >>= \case
        (Nothing, events)     -> evaluateInternal flag user store >>= (\x -> pure (x, events))
        (Just reason, events) -> pure (getOffValue flag reason, events)
    else pure (getOffValue flag EvaluationReasonOff, [])

status :: Prerequisite -> EvaluationDetail a -> Flag -> Bool
status prereq result prereqFlag = getField @"on" prereqFlag && (getField @"variationIndex" result) ==
    pure (getField @"variation" prereq)

checkPrerequisite :: (Monad m, LaunchDarklyStoreRead store m) => store -> UserI -> Flag -> Prerequisite
    -> m (Maybe EvaluationReason, [EvalEvent])
checkPrerequisite store user flag prereq = getFlagC store (getField @"key" prereq) >>= \case
    Left err                -> pure (pure $ EvaluationReasonError $ EvalErrorExternalStore err, [])
    Right Nothing           -> pure (pure $ EvaluationReasonPrerequisiteFailed (getField @"key" prereq), [])
    Right (Just prereqFlag) -> evaluateDetail prereqFlag user store >>= \(r, events) -> let
        event = newSuccessfulEvalEvent prereqFlag (getField @"variationIndex" r) (getField @"value" r) Nothing
            (getField @"reason" r) (Just $ getField @"key" flag)
        in if status prereq r prereqFlag then pure (Nothing, event : events) else
            pure (pure $ EvaluationReasonPrerequisiteFailed (getField @"key" prereq), event : events)

sequenceUntil :: Monad m => (a -> Bool) -> [m a] -> m [a]
sequenceUntil _ []     = return []
sequenceUntil p (m:ms) = m >>= \a -> if p a then return [a] else
    sequenceUntil p ms >>= \as -> return (a:as)

checkPrerequisites :: (Monad m, LaunchDarklyStoreRead store m) => Flag -> UserI -> store
    -> m (Maybe EvaluationReason, [EvalEvent])
checkPrerequisites flag user store = let p = getField @"prerequisites" flag in if null p then pure (Nothing, []) else do
    evals <- sequenceUntil (isJust . fst) $ map (checkPrerequisite store user flag) p
    pure (msum $ map fst evals, concatMap snd evals)

evaluateInternal :: (Monad m, LaunchDarklyStoreRead store m) => Flag -> UserI -> store -> m (EvaluationDetail Value)
evaluateInternal flag user store = result where
    checkTarget target = if elem (getField @"key" user) (getField @"values" target)
        then Just $ getVariation flag (getField @"variation" target) EvaluationReasonTargetMatch else Nothing
    checkRule (ruleIndex, rule) = ifM (ruleMatchesUser rule user store)
        (pure $ Just $ getValueForVariationOrRollout flag (getField @"variationOrRollout" rule) user
            EvaluationReasonRuleMatch { ruleIndex = ruleIndex, ruleId = getField @"id" rule, inExperiment = False })
        (pure Nothing)
    fallthrough = getValueForVariationOrRollout flag (getField @"fallthrough" flag) user (EvaluationReasonFallthrough False)
    result = let
        ruleMatch   = checkRule <$> zip [0..] (getField @"rules" flag)
        targetMatch = return . checkTarget <$> getField @"targets" flag
        in fromMaybe fallthrough <$> firstJustM Prelude.id (ruleMatch ++ targetMatch)

errorDefault :: EvalErrorKind -> Value -> EvaluationDetail Value
errorDefault kind v = EvaluationDetail { value = v, variationIndex = mzero, reason = EvaluationReasonError kind }

errorDetail :: EvalErrorKind -> EvaluationDetail Value
errorDetail kind = errorDefault kind Null

getValueForVariationOrRollout :: Flag -> VariationOrRollout -> UserI -> EvaluationReason -> EvaluationDetail Value
getValueForVariationOrRollout flag vr user reason =
    case variationIndexForUser vr user (getField @"key" flag) (getField @"salt" flag) of
        (Nothing, _)           -> errorDetail EvalErrorKindMalformedFlag
        (Just x, inExperiment) -> (getVariation flag x reason) & field @"reason" %~ setInExperiment inExperiment

setInExperiment :: Bool -> EvaluationReason -> EvaluationReason
setInExperiment inExperiment reason = case reason of
    EvaluationReasonFallthrough _         -> EvaluationReasonFallthrough inExperiment
    EvaluationReasonRuleMatch index idx _ -> EvaluationReasonRuleMatch index idx inExperiment
    x                                     -> x

ruleMatchesUser :: Monad m => LaunchDarklyStoreRead store m => Rule -> UserI -> store -> m Bool
ruleMatchesUser rule user store =
    allM (\clause -> clauseMatchesUser store clause user) (getField @"clauses" rule)

variationIndexForUser :: VariationOrRollout -> UserI -> Text -> Text -> (Maybe Integer, Bool)
variationIndexForUser vor user key salt
    | (Just variation) <- getField @"variation" vor = (pure variation, False)
    | (Just rollout) <- getField @"rollout" vor = let
        isExperiment = (getField @"kind" rollout) == RolloutKindExperiment
        variations = getField @"variations" rollout
        bucket = bucketUser user key (fromMaybe "key" $ getField @"bucketBy" rollout) salt (getField @"seed" rollout)
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

bucketUser :: UserI -> Text -> Text -> Text -> Maybe Int -> Float
bucketUser user key attribute salt seed = fromMaybe 0 $ do
    let secondarySuffix = maybe "" (T.append ".") $ getField @"secondary" user
    i <- valueOf user attribute >>= bucketableStringValue >>= \x -> pure $ B.take 15 $ B16.encode $ hash $ encodeUtf8 $
        case seed of
            Nothing    -> T.concat [key, ".", salt, ".", x, secondarySuffix]
            Just seed' -> T.concat [T.pack $ show seed', ".", x, secondarySuffix]
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

matchAny :: (Value -> Value -> Bool) -> Value -> [Value] -> Bool
matchAny op value = any (op value)

clauseMatchesUserNoSegments :: Clause -> UserI -> Bool
clauseMatchesUserNoSegments clause user = case valueOf user $ getField @"attribute" clause of
    Nothing        -> False
    Just (Array a) -> maybeNegate clause $ V.any (\x -> matchAny f x v) a
    Just x         -> maybeNegate clause $ matchAny f x v
    where
        f = getOperation $ getField @"op" clause
        v = getField @"values" clause

clauseMatchesUser :: (Monad m, LaunchDarklyStoreRead store m) => store -> Clause -> UserI -> m Bool
clauseMatchesUser store clause user
    | getField @"op" clause == OpSegmentMatch = do
        let values = [ x | String x <- getField @"values" clause]
        x <- anyM (\k -> getSegmentC store k >>= pure . checkSegment) values
        pure $ maybeNegate clause x
    | otherwise = pure $ clauseMatchesUserNoSegments clause user
    where
        checkSegment :: Either Text (Maybe Segment) -> Bool
        checkSegment (Right (Just segment)) = segmentContainsUser segment user
        checkSegment _                      = False

-- Segment ---------------------------------------------------------------------

segmentRuleMatchesUser :: SegmentRule -> UserI -> Text -> Text -> Bool
segmentRuleMatchesUser rule user key salt = (&&)
    (all (flip clauseMatchesUserNoSegments user) (getField @"clauses" rule))
    (flip (maybe True) (getField @"weight" rule) $ \weight ->
        bucketUser user key (fromMaybe "key" $ getField @"bucketBy" rule) salt Nothing < weight / 100000.0)

segmentContainsUser :: Segment -> UserI -> Bool
segmentContainsUser segment user
    | elem (getField @"key" user) (getField @"included" segment) = True
    | elem (getField @"key" user) (getField @"excluded" segment) = False
    | Just _ <- find
        (\r -> segmentRuleMatchesUser r user (getField @"key" segment) (getField @"salt" segment))
        (getField @"rules" segment) = True
    | otherwise = False
