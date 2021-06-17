module LaunchDarkly.Server.Features where

import Control.Monad                 (mzero)
import Data.Aeson                    (FromJSON(..), ToJSON(..), Value(..), withObject, (.:), (.:?), object, (.=), (.!=))
import Data.Text                     (Text)
import Data.HashSet                  (HashSet)
import Data.Generics.Product         (getField)
import GHC.Natural                   (Natural)
import GHC.Generics                  (Generic)

import LaunchDarkly.Server.Operators (Op)

data Target = Target
    { values    :: ![Text]
    , variation :: !Natural
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
    { variation :: !Natural
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
    { variation :: !(Maybe Natural)
    , rollout   :: !(Maybe Rollout)
    } deriving (Generic, FromJSON, ToJSON, Show, Eq)

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
    , offVariation           :: !(Maybe Natural)
    , variations             :: ![Value]
    , debugEventsUntilDate   :: !(Maybe Natural)
    } deriving (Generic, ToJSON, FromJSON, Show, Eq)

data Prerequisite = Prerequisite
    { key       :: !Text
    , variation :: !Natural
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
