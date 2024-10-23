{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module LaunchDarkly.Server.Evaluate where

import Control.Lens ((%~))
import Control.Monad (msum, mzero)
import Control.Monad.Extra (firstJustM)
import Crypto.Hash.SHA1 (hash)
import Data.Aeson.Types (Value (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Either (fromLeft)
import Data.Function ((&))
import Data.Generics.Product (field, getField)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List (genericIndex)
import Data.Maybe (fromJust, fromMaybe, isJust, mapMaybe)
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import GHC.Natural (Natural)

import Data.Either.Extra (mapRight)
import Data.Foldable (foldlM)
import Data.List.Extra (firstJust)
import LaunchDarkly.Server.Client.Internal (Client, Status (Initialized), getStatusI)
import LaunchDarkly.Server.Context (getIndividualContext, getValueForReference)
import LaunchDarkly.Server.Context.Internal (Context (Invalid), getKey, getKinds)
import LaunchDarkly.Server.Details (EvalErrorKind (..), EvaluationDetail (..), EvaluationReason (..))
import LaunchDarkly.Server.Events (EvalEvent, newSuccessfulEvalEvent, newUnknownFlagEvent, processEvalEvents)
import LaunchDarkly.Server.Features (Clause, Flag, Prerequisite, RolloutKind (RolloutKindExperiment), Rule, Segment (..), SegmentRule, SegmentTarget (..), Target, VariationOrRollout)
import LaunchDarkly.Server.Operators (Op (OpSegmentMatch), getOperation)
import LaunchDarkly.Server.Reference (getComponents, getError, isValid, makeLiteral, makeReference)
import LaunchDarkly.Server.Store.Internal (LaunchDarklyStoreRead, getFlagC, getSegmentC)
import LaunchDarkly.Server.Util (trd, snd3, fst3)

setFallback :: EvaluationDetail Value -> Value -> EvaluationDetail Value
setFallback detail fallback = case getField @"variationIndex" detail of
    Nothing -> detail {value = fallback}
    _ -> detail

setValue :: EvaluationDetail Value -> a -> EvaluationDetail a
setValue x v = x {value = v}

isError :: EvaluationReason -> Bool
isError reason = case reason of (EvaluationReasonError _) -> True; _ -> False

evaluateTyped :: Client -> Text -> Context -> a -> (a -> Value) -> Bool -> (Value -> Maybe a) -> IO (EvaluationDetail a)
evaluateTyped client key context fallback wrap includeReason convert =
    getStatusI client >>= \status ->
        if status /= Initialized
            then pure $ EvaluationDetail fallback Nothing $ EvaluationReasonError EvalErrorClientNotReady
            else
                evaluateInternalClient client key context (wrap fallback) includeReason >>= \(detail, _) ->
                    pure $
                        maybe
                            (EvaluationDetail fallback Nothing $ if isError (getField @"reason" detail) then (getField @"reason" detail) else EvaluationReasonError EvalErrorWrongType)
                            (setValue detail)
                            (convert $ getField @"value" detail)

evaluateInternalClient :: Client -> Text -> Context -> Value -> Bool -> IO (EvaluationDetail Value, [Text])
evaluateInternalClient _ _ (Invalid _) fallback _ = pure $ (errorDefault EvalErrorInvalidContext fallback, [])
evaluateInternalClient client key context fallback includeReason = do
    (detail, unknown, events, prereqs) <-
        getFlagC (getField @"store" client) key >>= \case
            Left err -> do
                let event = newUnknownFlagEvent key fallback (EvaluationReasonError $ EvalErrorExternalStore err) context
                pure (errorDetail $ EvalErrorExternalStore err, True, pure event, [])
            Right Nothing -> do
                let event = newUnknownFlagEvent key fallback (EvaluationReasonError EvalErrorFlagNotFound) context
                pure (errorDefault EvalErrorFlagNotFound fallback, True, pure event, [])
            Right (Just flag) -> do
                (detail, events, prereqs) <- evaluateDetail flag context HS.empty $ getField @"store" client
                let detail' = setFallback detail fallback
                pure
                    ( detail'
                    , False
                    , flip (:) events $
                        newSuccessfulEvalEvent
                            flag
                            (getField @"variationIndex" detail')
                            (getField @"value" detail')
                            (Just fallback)
                            (getField @"reason" detail')
                            Nothing
                            context
                    , prereqs
                    )
    processEvalEvents (getField @"config" client) (getField @"events" client) context includeReason events unknown
    pure (detail, prereqs)

getOffValue :: Flag -> EvaluationReason -> EvaluationDetail Value
getOffValue flag reason = case getField @"offVariation" flag of
    Just offVariation -> getVariation flag offVariation reason
    Nothing -> EvaluationDetail {value = Null, variationIndex = mzero, reason = reason}

getVariation :: Flag -> Integer -> EvaluationReason -> EvaluationDetail Value
getVariation flag index reason
    | idx < 0 = EvaluationDetail {value = Null, variationIndex = mzero, reason = EvaluationReasonError EvalErrorKindMalformedFlag}
    | idx >= length variations = EvaluationDetail {value = Null, variationIndex = mzero, reason = EvaluationReasonError EvalErrorKindMalformedFlag}
    | otherwise = EvaluationDetail {value = genericIndex variations index, variationIndex = pure index, reason = reason}
  where
    idx = fromIntegral index
    variations = getField @"variations" flag

evaluateDetail :: (Monad m, LaunchDarklyStoreRead store m) => Flag -> Context -> HS.HashSet Text -> store -> m (EvaluationDetail Value, [EvalEvent], [Text])
evaluateDetail flag@(getField @"on" -> False) _ _ _ = pure (getOffValue flag EvaluationReasonOff, [], [])
evaluateDetail flag context seenFlags store
    | HS.member (getField @"key" flag) seenFlags = pure (getOffValue flag $ EvaluationReasonError EvalErrorKindMalformedFlag, [], [])
    | otherwise =
        checkPrerequisites flag context (HS.insert (getField @"key" flag) seenFlags) store >>= \case
            (Nothing, events, prereqs) -> evaluateInternal flag context store >>= (\x -> pure (x, events, prereqs))
            (Just detail, events, prereqs) -> pure (detail, events, prereqs)

status :: Prerequisite -> EvaluationDetail a -> Flag -> Bool
status prereq result prereqFlag =
    getField @"on" prereqFlag
        && (getField @"variationIndex" result)
            == pure (getField @"variation" prereq)

sequenceUntil :: Monad m => (a -> Bool) -> [m a] -> m [a]
sequenceUntil _ [] = return []
sequenceUntil p (m : ms) =
    m >>= \a ->
        if p a
            then return [a]
            else sequenceUntil p ms >>= \as -> return (a : as)

checkPrerequisites :: (Monad m, LaunchDarklyStoreRead store m) => Flag -> Context -> HS.HashSet Text -> store -> m (Maybe (EvaluationDetail Value), [EvalEvent], [Text])
checkPrerequisites flag context seenFlags store =
    let p = getField @"prerequisites" flag
     in if null p
            then pure (Nothing, [], [])
            else do
                evals <- sequenceUntil (isJust . (\(a, _, _) -> a)) $ map (checkPrerequisite store context flag seenFlags) p
                pure (msum $ map fst3 evals, concatMap snd3 evals, mapMaybe trd evals)

checkPrerequisite :: (Monad m, LaunchDarklyStoreRead store m) => store -> Context -> Flag -> HS.HashSet Text -> Prerequisite -> m (Maybe (EvaluationDetail Value), [EvalEvent], Maybe Text)
checkPrerequisite store context flag seenFlags prereq =
    if HS.member prereqKey seenFlags
        then pure (Just $ errorDetail EvalErrorKindMalformedFlag, [], Nothing)
        else
            getFlagC store prereqKey >>= \case
                Left err -> pure (pure $ getOffValue flag $ EvaluationReasonError $ EvalErrorExternalStore err, [], Nothing)
                Right Nothing -> pure (pure $ getOffValue flag $ EvaluationReasonPrerequisiteFailed prereqKey, [], Just prereqKey)
                Right (Just prereqFlag) -> evaluateDetail prereqFlag context seenFlags store >>= (process prereqFlag)
  where
    prereqKey = getField @"key" prereq
    process prereqFlag (detail, events, _prereqs)
        | isError (getField @"reason" detail) = pure (Just $ errorDetail EvalErrorKindMalformedFlag, mempty, Just prereqKey)
        | otherwise =
            let event = newSuccessfulEvalEvent prereqFlag (getField @"variationIndex" detail) (getField @"value" detail) Nothing (getField @"reason" detail) (Just $ getField @"key" flag) context
                prereqKey = getField @"key" prereqFlag
             in if status prereq detail prereqFlag
                    then pure (Nothing, event : events, Just prereqKey)
                    else pure (pure $ getOffValue flag $ EvaluationReasonPrerequisiteFailed (getField @"key" prereq), event : events, Just prereqKey)

evaluateInternal :: (Monad m, LaunchDarklyStoreRead store m) => Flag -> Context -> store -> m (EvaluationDetail Value)
evaluateInternal flag context store = result
  where
    fallthrough = getValueForVariationOrRollout flag (getField @"fallthrough" flag) context (EvaluationReasonFallthrough False)
    result =
        let
            targetEvaluationResults = pure <$> checkTargets context flag
            ruleEvaluationResults = (checkRule flag context store) <$> zip [0 ..] (getField @"rules" flag)
         in
            fromMaybe fallthrough <$> firstJustM Prelude.id (targetEvaluationResults ++ ruleEvaluationResults)

checkRule :: (Monad m, LaunchDarklyStoreRead store m) => Flag -> Context -> store -> (Natural, Rule) -> m (Maybe (EvaluationDetail Value))
checkRule flag context store (ruleIndex, rule) =
    ruleMatchesContext rule context store
        >>= pure . \case
            Left _ -> Just $ errorDetail EvalErrorKindMalformedFlag
            Right True -> Just $ getValueForVariationOrRollout flag (getField @"variationOrRollout" rule) context EvaluationReasonRuleMatch {ruleIndex = ruleIndex, ruleId = getField @"id" rule, inExperiment = False}
            Right False -> Nothing

checkTargets :: Context -> Flag -> [Maybe (EvaluationDetail Value)]
checkTargets context flag =
    let userTargets = getField @"targets" flag
        contextTargets = getField @"contextTargets" flag
     in case contextTargets of
            [] -> checkTarget context "user" flag <$> userTargets
            _ -> checkContextTargets context flag userTargets contextTargets

checkContextTargets :: Context -> Flag -> [Target] -> [Target] -> [Maybe (EvaluationDetail Value)]
checkContextTargets context flag userTargets contextTargets =
    checkContextTarget context flag userTargets <$> contextTargets

checkContextTarget :: Context -> Flag -> [Target] -> Target -> Maybe (EvaluationDetail Value)
checkContextTarget context flag userTargets contextTarget =
    let contextKind = getField @"contextKind" contextTarget
        values = getField @"values" contextTarget
     in if contextKind == "user" && HS.null values
            then -- If the context target doesn't have any values specified, we are supposed to fall back to the user targets
                firstJust Prelude.id $ (checkTarget context "user" flag) <$> userTargets
            else checkTarget context contextKind flag contextTarget

checkTarget :: Context -> Text -> Flag -> Target -> Maybe (EvaluationDetail Value)
checkTarget context contextKind flag target =
    case getIndividualContext contextKind context of
        Nothing -> Nothing
        Just ctx ->
            if elem (getKey ctx) (getField @"values" target)
                then Just $ getVariation flag (getField @"variation" target) EvaluationReasonTargetMatch
                else Nothing

errorDefault :: EvalErrorKind -> Value -> EvaluationDetail Value
errorDefault kind v = EvaluationDetail {value = v, variationIndex = mzero, reason = EvaluationReasonError kind}

errorDetail :: EvalErrorKind -> EvaluationDetail Value
errorDetail kind = errorDefault kind Null

getValueForVariationOrRollout :: Flag -> VariationOrRollout -> Context -> EvaluationReason -> EvaluationDetail Value
getValueForVariationOrRollout flag vr context reason =
    case variationIndexForContext vr context (getField @"key" flag) (getField @"salt" flag) of
        (Nothing, _) -> errorDetail EvalErrorKindMalformedFlag
        (Just x, inExperiment) -> (getVariation flag x reason) & field @"reason" %~ setInExperiment inExperiment

setInExperiment :: Bool -> EvaluationReason -> EvaluationReason
setInExperiment inExperiment reason = case reason of
    EvaluationReasonFallthrough _ -> EvaluationReasonFallthrough inExperiment
    EvaluationReasonRuleMatch index idx _ -> EvaluationReasonRuleMatch index idx inExperiment
    x -> x

ruleMatchesContext :: Monad m => LaunchDarklyStoreRead store m => Rule -> Context -> store -> m (Either Text Bool)
ruleMatchesContext rule context store = foldlM (checkRule store context) (Right True) clauses
  where
    clauses = getField @"clauses" rule
    checkRule :: Monad m => LaunchDarklyStoreRead store m => store -> Context -> Either Text Bool -> Clause -> m (Either Text Bool)
    checkRule _ _ (Left e) _ = pure $ Left e
    checkRule _ _ (Right False) _ = pure $ Right False
    checkRule store context _ clause = clauseMatchesContext store clause context HS.empty

variationIndexForContext :: VariationOrRollout -> Context -> Text -> Text -> (Maybe Integer, Bool)
variationIndexForContext vor context key salt
    | (Just variation) <- getField @"variation" vor = (pure variation, False)
    | (Just rollout) <- getField @"rollout" vor =
        let
            isRolloutExperiment = (getField @"kind" rollout) == RolloutKindExperiment
            bucketBy = fromMaybe "key" $ if isRolloutExperiment then Nothing else getField @"bucketBy" rollout
            variations = getField @"variations" rollout
            bucket = bucketContext context (getField @"contextKind" rollout) key bucketBy salt (getField @"seed" rollout)
            isExperiment = isRolloutExperiment && (isJust bucket)
            c acc i =
                acc >>= \acc ->
                    let t = acc + ((getField @"weight" i) / 100000.0)
                     in case bucket of
                            Just v | v >= t -> Right t
                            _ -> Left (Just $ getField @"variation" i, (not $ getField @"untracked" i) && isExperiment)
         in
            if null variations
                then (Nothing, False)
                else
                    fromLeft
                        (Just $ getField @"variation" $ last variations, (not $ getField @"untracked" $ last variations) && isExperiment)
                        $ foldl c (Right (0.0 :: Float)) variations
    | otherwise = (Nothing, False)

-- Bucketing -------------------------------------------------------------------

hexCharToNumber :: Word8 -> Maybe Natural
hexCharToNumber w =
    fmap fromIntegral $
        if
                | 48 <= w && w <= 57 -> pure $ w - 48
                | 65 <= w && w <= 70 -> pure $ w - 55
                | 97 <= w && w <= 102 -> pure $ w - 87
                | otherwise -> Nothing

hexStringToNumber :: ByteString -> Maybe Natural
hexStringToNumber bytes = B.foldl' step (Just 0) bytes
  where
    step acc x = acc >>= \acc' -> hexCharToNumber x >>= pure . (+) (acc' * 16)

bucketContext :: Context -> Maybe Text -> Text -> Text -> Text -> Maybe Int -> Maybe Float
bucketContext context kind key attribute salt seed =
    let bucketBy = case kind of
            Nothing -> makeLiteral attribute
            Just _ -> makeReference attribute
     in case getIndividualContext (fromMaybe "user" kind) context of
            Nothing -> Nothing
            Just ctx ->
                let bucketableString = bucketableStringValue $ getValueForReference bucketBy ctx
                 in Just $ calculateBucketValue bucketableString key salt seed

calculateBucketValue :: (Maybe Text) -> Text -> Text -> Maybe Int -> Float
calculateBucketValue Nothing _ _ _ = 0
calculateBucketValue (Just text) key salt seed =
    let seed' = case seed of
            Nothing -> T.concat [key, ".", salt, ".", text]
            Just seed' -> T.concat [T.pack $ show seed', ".", text]
        byteString = B.take 15 $ B16.encode $ hash $ encodeUtf8 $ seed'
     in ((fromIntegral $ fromJust $ hexStringToNumber byteString) :: Float) / 0xFFFFFFFFFFFFFFF

floatingOrInteger' :: Scientific -> Either Double Integer
floatingOrInteger' = floatingOrInteger

bucketableStringValue :: Value -> Maybe Text
bucketableStringValue (String x) = pure x
bucketableStringValue (Number s) = either (const Nothing) (pure . T.pack . show) (floatingOrInteger' s)
bucketableStringValue _ = Nothing

-- Clause ----------------------------------------------------------------------

maybeNegate :: Clause -> Bool -> Bool
maybeNegate clause value = if getField @"negate" clause then not value else value

-- For a given clause, determine if the provided value matches that clause.
--
-- The operation to be check and the values to compare against are both extract from within the Clause itself.
matchAnyClauseValue :: Clause -> Value -> Bool
matchAnyClauseValue clause contextValue = any (f contextValue) v
  where
    f = getOperation $ getField @"op" clause
    v = getField @"values" clause

-- If attribute is "kind", then we treat operator and values as a match expression against a list of all individual
-- kinds in the context. That is, for a multi-kind context with kinds of "org" and "user", it is a match if either
-- of those strings is a match with Operator and Values.
clauseMatchesByKind :: Clause -> Context -> Bool
clauseMatchesByKind clause context = foldr f False (getKinds context)
  where
    f kind result
        | result == True = True
        | otherwise = matchAnyClauseValue clause (String kind)

clauseMatchesContextNoSegments :: Clause -> Context -> Either Text Bool
clauseMatchesContextNoSegments clause context
    | isValid (getField @"attribute" clause) == False = Left $ getError $ getField @"attribute" clause
    | ["kind"] == getComponents (getField @"attribute" clause) = Right $ maybeNegate clause $ clauseMatchesByKind clause context
    | otherwise = case getIndividualContext (getField @"contextKind" clause) context of
        Nothing -> Right False
        Just ctx -> case getValueForReference (getField @"attribute" clause) ctx of
            Null -> Right False
            Array a -> Right $ maybeNegate clause $ any (matchAnyClauseValue clause) a
            x -> Right $ maybeNegate clause $ matchAnyClauseValue clause x

clauseMatchesContext :: (Monad m, LaunchDarklyStoreRead store m) => store -> Clause -> Context -> HS.HashSet Text -> m (Either Text Bool)
clauseMatchesContext store clause context seenSegments
    | getField @"op" clause == OpSegmentMatch =
        let values = [x | String x <- getField @"values" clause]
         in foldlM (checkSegment store context seenSegments) (Right False) values >>= pure . mapRight (maybeNegate clause)
    | otherwise = pure $ clauseMatchesContextNoSegments clause context

checkSegment :: (Monad m, LaunchDarklyStoreRead store m) => store -> Context -> HS.HashSet Text -> Either Text Bool -> Text -> m (Either Text Bool)
checkSegment _ _ _ (Left e) _ = pure $ Left e
checkSegment _ _ _ (Right True) _ = pure $ Right True
checkSegment store context seenSegments _ value =
    getSegmentC store value >>= \case
        Right (Just segment) -> segmentContainsContext store segment context seenSegments
        _ -> pure $ Right False

-- Segment ---------------------------------------------------------------------

segmentRuleMatchesContext :: (Monad m, LaunchDarklyStoreRead store m) => store -> SegmentRule -> Context -> Text -> Text -> HS.HashSet Text -> m (Either Text Bool)
segmentRuleMatchesContext store rule context key salt seenSegments =
    foldlM (checkClause store) (Right True) (getField @"clauses" rule) >>= \result ->
        pure $ case result of
            Left _ -> result
            Right False -> result
            _ ->
                Right $
                    ( flip (maybe True) (getField @"weight" rule) $ \weight ->
                        let bucket = bucketContext context (getField @"rolloutContextKind" rule) key (fromMaybe "key" $ getField @"bucketBy" rule) salt Nothing
                         in case bucket of
                                Just v | v >= (weight / 100000.0) -> False
                                _ -> True
                    )
  where
    checkClause :: (Monad m, LaunchDarklyStoreRead store m) => store -> Either Text Bool -> Clause -> m (Either Text Bool)
    checkClause _ (Left e) _ = pure $ Left e
    checkClause _ (Right False) _ = pure $ Right False
    checkClause store _ clause = clauseMatchesContext store clause context seenSegments

segmentContainsContext :: (Monad m, LaunchDarklyStoreRead store m) => store -> Segment -> Context -> HS.HashSet Text -> m (Either Text Bool)
segmentContainsContext store (Segment {included, includedContexts, excluded, excludedContexts, key, salt, rules}) context seenSegments
    | HS.member key seenSegments = pure $ Left "segment rule caused a circular reference; this is probably a temporary condition"
    | contextKeyInTargetList included "user" context = pure $ Right True
    | (any (flip contextKeyInSegmentTarget context) includedContexts) = pure $ Right True
    | contextKeyInTargetList excluded "user" context = pure $ Right False
    | (any (flip contextKeyInSegmentTarget context) excludedContexts) = pure $ Right False
    | otherwise = foldlM (checkRules store) (Right False) rules
  where
    checkRules :: (Monad m, LaunchDarklyStoreRead store m) => store -> Either Text Bool -> SegmentRule -> m (Either Text Bool)
    checkRules _ (Left e) _ = pure $ Left e
    checkRules _ (Right True) _ = pure $ Right True
    checkRules store _ rule = segmentRuleMatchesContext store rule context key salt (HS.insert key seenSegments)

contextKeyInSegmentTarget :: SegmentTarget -> Context -> Bool
contextKeyInSegmentTarget (SegmentTarget {values, contextKind}) = contextKeyInTargetList values contextKind

contextKeyInTargetList :: (HashSet Text) -> Text -> Context -> Bool
contextKeyInTargetList targets kind context = case getIndividualContext kind context of
    Just ctx -> elem (getKey ctx) targets
    Nothing -> False
