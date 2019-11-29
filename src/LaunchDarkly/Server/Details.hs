module LaunchDarkly.Server.Details where

import           Data.Aeson.Types       (Value(..), ToJSON, toJSON)
import qualified Data.HashMap.Strict as HM
import           Data.Text              (Text)
import           GHC.Natural            (Natural)
import           GHC.Generics           (Generic)

data EvaluationDetail value = EvaluationDetail
    { value          :: value
    , variationIndex :: Maybe Natural
    , reason         :: EvaluationReason
    } deriving (Generic, Eq, Show)

instance ToJSON a => ToJSON (EvaluationDetail a) where
    toJSON = toJSON

data EvaluationReason
    = EvaluationReasonOff
    | EvaluationReasonTargetMatch
    | EvaluationReasonRuleMatch
          { ruleIndex :: Natural
          , ruleId    :: Text
          }
    | EvaluationReasonPrerequisiteFailed
          { prerequisiteKey :: Text
          }
    | EvaluationReasonFallthrough
    | EvaluationReasonError
          { errorKind :: EvalErrorKind
          }
    deriving (Generic, Eq, Show)

instance ToJSON EvaluationReason where
    toJSON x = case x of
        EvaluationReasonOff                                  ->
            Object $ HM.fromList [("kind", "OFF")]
        EvaluationReasonTargetMatch                          ->
            Object $ HM.fromList [("kind", "TARGET_MATCH")]
        (EvaluationReasonRuleMatch ruleIndex ruleId)         ->
            Object $ HM.fromList [("kind", "RULE_MATCH"), ("ruleIndex", toJSON ruleIndex), ("ruleId", toJSON ruleId)]
        (EvaluationReasonPrerequisiteFailed prerequisiteKey) ->
            Object $ HM.fromList [("kind", "PREREQUISITE_FAILED"), ("prerequisiteKey", toJSON prerequisiteKey)]
        EvaluationReasonFallthrough                          ->
            Object $ HM.fromList [("kind", "FALLTHROUGH")]
        (EvaluationReasonError errorKind)                    ->
            Object $ HM.fromList [("kind", "ERROR"), ("errorKind", toJSON errorKind)]

data EvalErrorKind
    = EvalErrorKindMalformedFlag
    | EvalErrorFlagNotFound
    | EvalErrorWrongType
    | EvalErrorUserNotSpecified
    deriving (Generic, Eq, Show)

instance ToJSON EvalErrorKind where
    toJSON x = String $ case x of
        EvalErrorKindMalformedFlag -> "MALFORMED_FLAG"
        EvalErrorFlagNotFound      -> "FLAG_NOT_FOUND"
        EvalErrorWrongType         -> "WRONG_TYPE"
        EvalErrorUserNotSpecified  -> "USER_NOT_SPECIFIED"
