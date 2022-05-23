module LaunchDarkly.Server.Features where

import Control.Lens                  (element, (^?))
import Control.Monad                 (mzero)
import Data.Aeson                    (FromJSON(..), ToJSON(..), Value(..), withObject, (.:), (.:?), object, (.=), (.!=))
import Data.Maybe                    (fromMaybe)
import Data.Text                     (Text)
import Data.HashSet                  (HashSet)
import Data.Generics.Product         (getField)
import GHC.Natural                   (Natural)
import GHC.Generics                  (Generic)

import LaunchDarkly.Server.Operators (Op)
import LaunchDarkly.Server.Details (EvaluationReason (..))
import qualified LaunchDarkly.Server.Details as D

data Target = Target
    { values    :: ![Text]
    , variation :: !Integer
    } deriving (Generic, FromJSON, ToJSON, Show, Eq)

data Rule = Rule
    { id                 :: !Text
    , clauses            :: ![Clause]
    , variationOrRollout :: !VariationOrRollout
    , trackEvents        :: !Bool
    } deriving (Generic, Show, Eq)

instance FromJSON Rule where
    parseJSON = withObject "Rule" $ \o -> do
        id          <- o .:  "id"
        clauses     <- o .:  "clauses"
        variation   <- o .:? "variation"
        rollout     <- o .:? "rollout"
        trackEvents <- o .:  "trackEvents"
        pure Rule
            { id                 = id
            , clauses            = clauses
            , variationOrRollout = VariationOrRollout
                { variation = variation
                , rollout   = rollout
                }
            , trackEvents        = trackEvents
            }

instance ToJSON Rule where
    toJSON rule = object
        [ "id"          .= getField @"id" rule
        , "clauses"     .= getField @"clauses" rule
        , "trackEvents" .= getField @"trackEvents" rule
        , "variation"   .= getField @"variation" (getField @"variationOrRollout" rule)
        , "rollout"     .= getField @"rollout" (getField @"variationOrRollout" rule)
        ]

data WeightedVariation = WeightedVariation
    { variation :: !Integer
    , weight    :: !Float
    , untracked :: !Bool
    } deriving (Generic, ToJSON, Show, Eq)

instance FromJSON WeightedVariation where
    parseJSON = withObject "WeightedVariation" $ \o -> do
        variation <- o .:  "variation"
        weight    <- o .:  "weight"
        untracked <- o .:? "untracked" .!= False
        pure WeightedVariation { .. }

data RolloutKind = RolloutKindExperiment | RolloutKindRollout
    deriving (Eq, Show)

instance ToJSON RolloutKind where
    toJSON x = String $ case x of
        RolloutKindExperiment -> "experiment"
        RolloutKindRollout    -> "rollout"

instance FromJSON RolloutKind where
    parseJSON x = case x of
        (String "experiment") -> pure RolloutKindExperiment
        (String "rollout")    -> pure RolloutKindRollout
        _                     -> mzero

data Rollout = Rollout
    { variations :: ![WeightedVariation]
    , bucketBy   :: !(Maybe Text)
    , kind       :: !RolloutKind
    , seed       :: !(Maybe Int)
    } deriving (Generic, ToJSON, Show, Eq)

instance FromJSON Rollout where
    parseJSON = withObject "rollout" $ \o -> do
        variations <- o .:  "variations"
        bucketBy   <- o .:? "bucketBy"
        kind       <- o .:? "kind" .!= RolloutKindRollout
        seed       <- o .:? "seed"
        pure Rollout { .. }

data VariationOrRollout = VariationOrRollout
    { variation :: !(Maybe Integer)
    , rollout   :: !(Maybe Rollout)
    } deriving (Generic, FromJSON, ToJSON, Show, Eq)

data ClientSideAvailability = ClientSideAvailability
    { usingEnvironmentId     :: !Bool
    , usingMobileKey         :: !Bool
    , explicit               :: !Bool
    } deriving (Generic, Show, Eq)

instance FromJSON ClientSideAvailability where
    parseJSON = withObject "ClientSideAvailability" $ \obj -> ClientSideAvailability
        <$> obj .: "usingEnvironmentId"
        <*> obj .: "usingMobileKey"
        <*> pure True

instance ToJSON ClientSideAvailability where
    toJSON (ClientSideAvailability env mob _) =
        object [ "usingEnvironmentId" .= env, "usingMobileKey" .= mob ]

data Flag = Flag
    { key                    :: !Text
    , version                :: !Natural
    , on                     :: !Bool
    , trackEvents            :: !Bool
    , trackEventsFallthrough :: !Bool
    , deleted                :: !Bool
    , prerequisites          :: ![Prerequisite]
    , salt                   :: !Text
    , targets                :: ![Target]
    , rules                  :: ![Rule]
    , fallthrough            :: !VariationOrRollout
    , offVariation           :: !(Maybe Integer)
    , variations             :: ![Value]
    , debugEventsUntilDate   :: !(Maybe Natural)
    , clientSideAvailability :: !ClientSideAvailability
    } deriving (Generic, Show, Eq)

instance ToJSON Flag where
    toJSON flag = object $
        [ "key" .= getField @"key" flag
        , "version" .= getField @"version" flag
        , "on" .= getField @"on" flag
        , "trackEvents" .= getField @"trackEvents" flag
        , "trackEventsFallthrough" .= getField @"trackEventsFallthrough" flag
        , "deleted" .= getField @"deleted" flag
        , "prerequisites" .= getField @"prerequisites" flag
        , "salt" .= getField @"salt" flag
        , "targets" .= getField @"targets" flag
        , "rules" .= getField @"rules" flag
        , "fallthrough" .= getField @"fallthrough" flag
        , "offVariation" .= getField @"offVariation" flag
        , "variations" .= getField @"variations" flag
        , "debugEventsUntilDate" .= getField @"debugEventsUntilDate" flag
        , "clientSide" .= (getField @"usingEnvironmentId" $ getField @"clientSideAvailability" flag)
        ] <> case getField @"explicit" $ getField @"clientSideAvailability" flag of
               True -> [ "clientSideAvailability" .= getField @"clientSideAvailability" flag ]
               False -> [ ]

instance FromJSON Flag where
    parseJSON = withObject "Flag" $ \obj -> do
        key <- obj .: "key"
        version <- obj .: "version"
        on <- obj .: "on"
        trackEvents <- obj .: "trackEvents"
        trackEventsFallthrough <- obj .: "trackEventsFallthrough"
        deleted <- obj .: "deleted"
        prerequisites <- obj .: "prerequisites"
        salt <- obj .: "salt"
        targets <- obj .: "targets"
        rules <- obj .: "rules"
        fallthrough <- obj .: "fallthrough"
        offVariation <- obj .:? "offVariation"
        variations <- obj .: "variations"
        debugEventsUntilDate <- obj .:? "debugEventsUntilDate"
        clientSide <- obj .:? "clientSide" .!= False
        clientSideAvailability <- obj .:? "clientSideAvailability" .!= ClientSideAvailability clientSide True False
        pure Flag { .. }

isClientSideOnlyFlag :: Flag -> Bool
isClientSideOnlyFlag flag = getField @"usingEnvironmentId" $ getField  @"clientSideAvailability" flag

-- If the reason for the flag is in an experiment,
-- or if it's a fallthrough reason and the flag has trackEventsFallthrough
-- or if it's a rule match and the rule that matched has track events turned on
-- otherwise false
isInExperiment :: Flag -> EvaluationReason -> Bool
isInExperiment _ reason
  | D.isInExperiment reason = True
isInExperiment flag EvaluationReasonFallthrough {..} = getField @"trackEventsFallthrough" flag
isInExperiment flag (EvaluationReasonRuleMatch ruleIndex _ _) =
    let index = fromIntegral ruleIndex
        rules = getField @"rules" flag
        rule = rules ^? element index
    in fromMaybe False $ fmap (getField @"trackEvents") rule
isInExperiment _ _ = False

data Prerequisite = Prerequisite
    { key       :: !Text
    , variation :: !Integer
    } deriving (Generic, FromJSON, ToJSON, Show, Eq)

data SegmentRule = SegmentRule
    { id       :: !Text
    , clauses  :: ![Clause]
    , weight   :: !(Maybe Float)
    , bucketBy :: !(Maybe Text)
    } deriving (Generic, FromJSON, ToJSON, Show, Eq)

data Segment = Segment
    { key      :: !Text
    , included :: !(HashSet Text)
    , excluded :: !(HashSet Text)
    , salt     :: !Text
    , rules    :: ![SegmentRule]
    , version  :: !Natural
    , deleted  :: !Bool
    } deriving (Generic, FromJSON, ToJSON, Show, Eq)

data Clause = Clause
    { attribute :: !Text
    , negate    :: !Bool
    , op        :: !Op
    , values    :: ![Value]
    } deriving (Generic, FromJSON, ToJSON, Show, Eq)
