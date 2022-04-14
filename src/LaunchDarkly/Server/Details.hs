{-# LANGUAGE OverloadedLists #-}
module LaunchDarkly.Server.Details where

import           Data.Aeson.Types       (Value(..), ToJSON, toJSON)
import           Data.Text              (Text)
import           GHC.Exts               (fromList)
import           GHC.Natural            (Natural)
import           GHC.Generics           (Generic)

-- | Combines the result of a flag evaluation with an explanation of how it was
-- calculated.
data EvaluationDetail value = EvaluationDetail
    { value          :: !value
      -- ^ The result of the flag evaluation. This will be either one of the
      -- flag's variations or the default value passed by the application.
    , variationIndex :: !(Maybe Natural)
      -- ^ The index of the returned value within the flag's list of variations,
      -- e.g. 0 for the first variation - or Nothing if the default value was
      -- returned.
    , reason         :: !EvaluationReason
      -- ^ Describes the main factor that influenced the flag evaluation value.
    } deriving (Generic, Eq, Show)

instance ToJSON a => ToJSON (EvaluationDetail a) where
    toJSON = toJSON

-- | Defines the possible values of the Kind property of EvaluationReason.
data EvaluationReason
    = EvaluationReasonOff
      -- ^ Indicates that the flag was off and therefore returned its configured
      -- off value.
    | EvaluationReasonTargetMatch
      -- ^ indicates that the user key was specifically targeted for this flag.
    | EvaluationReasonRuleMatch
          { ruleIndex    :: !Natural
            -- ^ The index of the rule that was matched (0 being the first).
          , ruleId       :: !Text
            -- ^ The unique identifier of the rule that was matched.
          , inExperiment :: !Bool
            -- ^ Whether the evaluation was part of an experiment. Is true if
            -- the evaluation resulted in an experiment rollout *and* served
            -- one of the variations in the experiment. Otherwise false.
          }
      -- ^ Indicates that the user matched one of the flag's rules.
    | EvaluationReasonPrerequisiteFailed
          { prerequisiteKey :: !Text
            -- ^ The flag key of the prerequisite that failed.
          }
      -- ^ Indicates that the flag was considered off because it had at least
      -- one prerequisite flag that either was off or did not return the desired
      -- variation.
    | EvaluationReasonFallthrough
          { inExperiment :: !Bool
            -- ^ Whether the evaluation was part of an experiment. Is
            -- true if the evaluation resulted in an experiment rollout *and*
            -- served one of the variations in the experiment. Otherwise false.
          }
      -- ^ Indicates that the flag was on but the user did not match any targets
      -- or rules.
    | EvaluationReasonError
          { errorKind :: !EvalErrorKind
            -- ^ Describes the type of error.
          }
      -- ^ Indicates that the flag could not be evaluated, e.g. because it does
      -- not exist or due to an unexpected error. In this case the result value
      -- will be the default value that the caller passed to the client.
    deriving (Generic, Eq, Show)

instance ToJSON EvaluationReason where
    toJSON x = case x of
        EvaluationReasonOff                                        ->
            Object $ fromList [("kind", "OFF")]
        EvaluationReasonTargetMatch                                ->
            Object $ fromList [("kind", "TARGET_MATCH")]
        (EvaluationReasonRuleMatch ruleIndex ruleId inExperiment)  ->
            Object $ fromList [("kind", "RULE_MATCH"), ("ruleIndex", toJSON ruleIndex), ("ruleId", toJSON ruleId), ("inExperiment", toJSON inExperiment)]
        (EvaluationReasonPrerequisiteFailed prerequisiteKey)       ->
            Object $ fromList [("kind", "PREREQUISITE_FAILED"), ("prerequisiteKey", toJSON prerequisiteKey)]
        EvaluationReasonFallthrough inExperiment                   ->
            Object $ fromList [("kind", "FALLTHROUGH"), ("inExperiment", toJSON inExperiment)]
        (EvaluationReasonError errorKind)                          ->
            Object $ fromList [("kind", "ERROR"), ("errorKind", toJSON errorKind)]

-- | Defines the possible values of the errorKind property of EvaluationReason.
data EvalErrorKind
    = EvalErrorKindMalformedFlag
      -- ^ Indicates that there was an internal inconsistency in the flag data,
      -- e.g. a rule specified a nonexistent variation.
    | EvalErrorFlagNotFound
      -- ^ Indicates that the caller provided a flag key that did not match any
      -- known flag.
    | EvalErrorWrongType
      -- ^ Indicates that the result value was not of the requested type, e.g.
      -- you called boolVariationDetail but the value was an integer.
    | EvalErrorClientNotReady
      -- ^ Indicates that the caller tried to evaluate a flag before the client
      -- had successfully initialized.
    | EvalErrorExternalStore !Text
      -- ^ Indicates that some error was returned by the external feature store.
    deriving (Generic, Eq, Show)

instance ToJSON EvalErrorKind where
    toJSON x = String $ case x of
        EvalErrorKindMalformedFlag -> "MALFORMED_FLAG"
        EvalErrorFlagNotFound      -> "FLAG_NOT_FOUND"
        EvalErrorWrongType         -> "WRONG_TYPE"
        EvalErrorClientNotReady    -> "CLIENT_NOT_READY"
        EvalErrorExternalStore _   -> "EXTERNAL_STORE_ERROR"
