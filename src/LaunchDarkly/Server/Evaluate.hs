module LaunchDarkly.Server.Evaluate where

import Control.Monad                       (mzero, msum)
import Control.Monad.Extra                 (ifM, anyM, allM, firstJustM)
import Crypto.Hash.SHA1                    (hash)
import Data.Scientific                     (Scientific, floatingOrInteger)
import Data.Either                         (either, fromLeft)
import Data.Aeson.Types                    (Value(..))
import Data.Maybe                          (maybe, fromJust, isJust)
import Data.Text                           (Text)
import Data.Generics.Product               (getField)
import Data.List                           (genericIndex, null, find)
import qualified Data.Vector as            V
import qualified Data.Text as              T
import qualified Data.ByteString as        B
import qualified Data.ByteString.Base16 as B16
import Data.Maybe                          (fromMaybe)
import Data.Text.Encoding                  (encodeUtf8)
import GHC.Natural                         (Natural, naturalToInt)
import Data.Word                           (Word8)
import Data.ByteString                     (ByteString)

import LaunchDarkly.Server.Client          (Client)
import LaunchDarkly.Server.User.Internal   (User, valueOf)
import LaunchDarkly.Server.Features        (Flag, Segment, Prerequisite, SegmentRule, Clause, VariationOrRollout, Rule)
import LaunchDarkly.Server.Store           (LaunchDarklyStoreRead, getFlag, getSegment)
import LaunchDarkly.Server.Operators       (Op(OpSegmentMatch), getOperation)
import LaunchDarkly.Server.Events          (EvalEvent, newUnknownFlagEvent, newSuccessfulEvalEvent, processEvalEvents)
import LaunchDarkly.Server.Details         (EvaluationDetail(..), EvaluationReason(..), EvalErrorKind(..))

setFallback :: EvaluationDetail Value -> Value -> EvaluationDetail Value
setFallback detail fallback = case getField @"variationIndex" detail of
    Nothing -> detail { value = fallback }; _ -> detail

setValue :: EvaluationDetail Value -> a -> EvaluationDetail a
setValue x v = x { value = v }

isError :: EvaluationReason -> Bool
isError reason = case reason of (EvaluationReasonError _) -> True; _ -> False

evaluateTyped :: Client -> Text -> User -> a -> (a -> Value) -> Bool -> (Value -> Maybe a) -> IO (EvaluationDetail a)
evaluateTyped client key user fallback wrap includeReason convert =
    evaluateInternalClient client key user (wrap fallback) includeReason >>= \r -> pure $ maybe
        (EvaluationDetail fallback Nothing $ if isError (getField @"reason" r)
            then (getField @"reason" r) else EvaluationReasonError EvalErrorWrongType)
        (\d -> setValue r d) (convert $ getField @"value" r)

evaluateInternalClient :: Client -> Text -> User -> Value -> Bool -> IO (EvaluationDetail Value)
evaluateInternalClient client key user fallback includeReason = do
    (reason, unknown, events) <- getFlag (getField @"store" client) key >>= \case
        Nothing -> do
            let event = newUnknownFlagEvent key fallback (EvaluationReasonError EvalErrorFlagNotFound)
            pure (errorDetail EvalErrorFlagNotFound, True, pure event)
        Just flag -> do
            (reason, events) <- case getField @"key" user of
                Nothing -> pure (errorDetail EvalErrorUserNotSpecified, [])
                Just _  -> evaluateDetail flag user $ getField @"store" client
            let reason' = setFallback reason fallback
            pure (reason', False, (flip (:)) events $ newSuccessfulEvalEvent flag (getField @"variationIndex" reason')
                (getField @"value" reason') (Just fallback) (getField @"reason" reason') Nothing)
    processEvalEvents (getField @"config" client) (getField @"events" client) user includeReason events unknown
    pure reason

getOffValue :: Flag -> EvaluationReason -> EvaluationDetail Value
getOffValue flag reason = case getField @"offVariation" flag of
    Just offVariation -> getVariation flag offVariation reason
    Nothing           -> EvaluationDetail { value = Null, variationIndex = mzero, reason = reason }

getVariation :: Flag -> Natural -> EvaluationReason -> EvaluationDetail Value
getVariation flag index reason = let variations = getField @"variations" flag in
    if naturalToInt index >= length variations
        then EvaluationDetail { value = Null, variationIndex = mzero, reason = reason }
        else EvaluationDetail { value = genericIndex variations index, variationIndex = pure index, reason = reason }

evaluateDetail :: (Monad m, LaunchDarklyStoreRead store m) => Flag -> User -> store
    -> m (EvaluationDetail Value, [EvalEvent])
evaluateDetail flag user store = if getField @"on" flag
    then checkPrerequisites flag user store >>= \case
        (Nothing, events)     -> evaluateInternal flag user store >>= (\x -> pure (x, events))
        (Just reason, events) -> pure (getOffValue flag reason, events)
    else pure (getOffValue flag EvaluationReasonOff, [])

status :: Prerequisite -> EvaluationDetail a -> Flag -> Bool
status prereq result prereqFlag = getField @"on" prereqFlag && (getField @"variationIndex" result) ==
    (pure $ getField @"variation" prereq)

checkPrerequisite :: (Monad m, LaunchDarklyStoreRead store m) => store -> User -> Flag -> Prerequisite
    -> m (Maybe EvaluationReason, [EvalEvent])
checkPrerequisite store user flag prereq = getFlag store (getField @"key" prereq) >>= \case
    Nothing         -> pure (pure $ EvaluationReasonPrerequisiteFailed (getField @"key" prereq), [])
    Just prereqFlag -> evaluateDetail prereqFlag user store >>= \(r, events) -> let
        event = newSuccessfulEvalEvent prereqFlag (getField @"variationIndex" r) (getField @"value" r) Nothing
            (getField @"reason" r) (Just $ getField @"key" flag)
        in if status prereq r prereqFlag then pure (Nothing, event : events) else
            pure (pure $ EvaluationReasonPrerequisiteFailed (getField @"key" prereq), event : events)

sequenceUntil :: Monad m => (a -> Bool) -> [m a] -> m [a]
sequenceUntil _ [] = return []
sequenceUntil p (m:ms) = m >>= \a -> if p a then return [a] else
    sequenceUntil p ms >>= \as -> return (a:as)

checkPrerequisites :: (Monad m, LaunchDarklyStoreRead store m) => Flag -> User -> store
    -> m (Maybe EvaluationReason, [EvalEvent])
checkPrerequisites flag user store = let p = getField @"prerequisites" flag in if null p then pure (Nothing, []) else do
    evals <- sequenceUntil (isJust . fst) $ map (checkPrerequisite store user flag) p
    pure (msum $ map fst evals, concat $ map snd evals)

evaluateInternal :: (Monad m, LaunchDarklyStoreRead store m) => Flag -> User -> store -> m (EvaluationDetail Value)
evaluateInternal flag user store = result where
    checkTarget target = if elem (getField @"key" user) (Just <$> getField @"values" target)
        then Just $ getVariation flag (getField @"variation" target) EvaluationReasonTargetMatch else Nothing
    checkRule (ruleIndex, rule) = ifM (ruleMatchesUser rule user store)
        (pure $ Just $ getValueForVariationOrRollout flag (getField @"variationOrRollout" rule) user
            EvaluationReasonRuleMatch { ruleIndex = ruleIndex, ruleId = getField @"id" rule })
        (pure Nothing)
    fallthrough = getValueForVariationOrRollout flag (getField @"fallthrough" flag) user EvaluationReasonFallthrough
    result = let
        ruleMatch   = checkRule <$> zip [0..] (getField @"rules" flag)
        targetMatch = return . checkTarget <$> getField @"targets" flag
        in fromMaybe fallthrough <$> firstJustM Prelude.id (ruleMatch ++ targetMatch)

errorDetail :: EvalErrorKind -> EvaluationDetail Value
errorDetail kind = EvaluationDetail { value = Null, variationIndex = mzero, reason = EvaluationReasonError kind }

getValueForVariationOrRollout :: Flag -> VariationOrRollout -> User -> EvaluationReason -> EvaluationDetail Value
getValueForVariationOrRollout flag vr user reason =
    case variationIndexForUser vr user (getField @"key" flag) (getField @"salt" flag) of
        Nothing -> errorDetail EvalErrorKindMalformedFlag
        Just x  -> getVariation flag x reason

ruleMatchesUser :: Monad m => LaunchDarklyStoreRead store m => Rule -> User -> store -> m Bool
ruleMatchesUser rule user store =
    allM (\clause -> clauseMatchesUser store clause user) (getField @"clauses" rule)

variationIndexForUser :: VariationOrRollout -> User -> Text -> Text -> Maybe Natural
variationIndexForUser vor user key salt
    | (Just variation) <- getField @"variation" vor = pure variation
    | (Just rollout) <- getField @"rollout" vor = let
        bucket = bucketUser user key (fromMaybe "key" $ getField @"bucketBy" rollout) salt
        c acc i = acc >>= \acc -> let t = acc + ((getField @"weight" i) / 100000.0) in
            if bucket < t then Left (Just $ getField @"variation" i) else Right t
        in fromLeft Nothing $ foldl c (Right (0.0 :: Float)) (getField @"variations" rollout)
    | otherwise = Nothing

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

bucketUser :: User -> Text -> Text -> Text -> Float
bucketUser user key attribute salt = fromMaybe 0 $ do
    i <- valueOf user attribute >>= bucketableStringValue >>= \x -> pure $ B.take 15 $ B16.encode $ hash $ encodeUtf8 $
        T.concat [key, ".", salt, ".", x, maybe "" (T.append ".") $ getField @"secondary" user]
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
matchAny op value list = any (op value) list

clauseMatchesUserNoSegments :: Clause -> User -> Bool
clauseMatchesUserNoSegments clause user = case valueOf user $ getField @"attribute" clause of
    Nothing        -> False
    Just (Array a) -> maybeNegate clause $ V.any (\x -> matchAny f x v) a
    Just x         -> maybeNegate clause $ matchAny f x v
    where
        f = getOperation $ getField @"op" clause
        v = getField @"values" clause

clauseMatchesUser :: (Monad m, LaunchDarklyStoreRead store m) => store -> Clause -> User -> m Bool
clauseMatchesUser store clause user
    | getField @"op" clause == OpSegmentMatch = do
        let values = [ x | String x <- getField @"values" clause]
        x <- anyM (\k -> getSegment store k >>= (pure . maybe False (flip segmentContainsUser user))) values
        pure $ maybeNegate clause x
    | otherwise = pure $ clauseMatchesUserNoSegments clause user

-- Segment ---------------------------------------------------------------------

segmentRuleMatchesUser :: SegmentRule -> User -> Text -> Text -> Bool
segmentRuleMatchesUser rule user key salt = (&&)
    (all (flip clauseMatchesUserNoSegments user) (getField @"clauses" rule))
    (flip (maybe True) (getField @"weight" rule) $ \weight ->
        bucketUser user key (fromMaybe "key" $ getField @"bucketBy" rule) salt < weight / 100000.0)

segmentContainsUser :: Segment -> User -> Bool
segmentContainsUser segment user
    | Nothing <- getField @"key" user = False
    | elem (fromJust $ getField @"key" user) (getField @"included" segment) = True
    | elem (fromJust $ getField @"key" user) (getField @"excluded" segment) = False
    | Just _ <- find
        (\r -> segmentRuleMatchesUser r user (getField @"key" segment) (getField @"salt" segment))
        (getField @"rules" segment) = True
    | otherwise = False
