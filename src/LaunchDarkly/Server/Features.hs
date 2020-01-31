module LaunchDarkly.Server.Features where

import Data.Aeson                    (FromJSON(..), ToJSON, Value(..), withObject, (.:), (.:?))
import Data.Text                     (Text)
import Data.HashSet                  (HashSet)
import GHC.Natural                   (Natural)
import GHC.Generics                  (Generic)

import LaunchDarkly.Server.Operators (Op)

data Target = Target
    { values    :: [Text]
    , variation :: Natural
    } deriving (Generic, FromJSON, ToJSON, Show, Eq)

data Rule = Rule
    { id                 :: Text
    , clauses            :: [Clause]
    , variationOrRollout :: VariationOrRollout
    , trackEvents        :: Bool
    } deriving (Generic, ToJSON, Show, Eq)

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

data WeightedVariation = WeightedVariation
    { variation :: Natural
    , weight    :: Float
    } deriving (Generic, FromJSON, ToJSON, Show, Eq)

data Rollout = Rollout
    { variations :: [WeightedVariation]
    , bucketBy   :: Maybe Text
    } deriving (Generic, FromJSON, ToJSON, Show, Eq)

data VariationOrRollout = VariationOrRollout
    { variation :: Maybe Natural
    , rollout   :: Maybe Rollout
    } deriving (Generic, FromJSON, ToJSON, Show, Eq)

data Flag = Flag
    { key                    :: Text
    , version                :: Natural
    , on                     :: Bool
    , trackEvents            :: Bool
    , trackEventsFallthrough :: Bool
    , deleted                :: Bool
    , prerequisites          :: [Prerequisite]
    , salt                   :: Text
    , sel                    :: Text
    , targets                :: [Target]
    , rules                  :: [Rule]
    , fallthrough            :: VariationOrRollout
    , offVariation           :: Maybe Natural
    , variations             :: [Value]
    , debugEventsUntilDate   :: Maybe Natural
    } deriving (Generic, ToJSON, FromJSON, Show, Eq)

data Prerequisite = Prerequisite
    { key       :: Text
    , variation :: Natural
    } deriving (Generic, FromJSON, ToJSON, Show, Eq)

data SegmentRule = SegmentRule
    { id       :: Text
    , clauses  :: [Clause]
    , weight   :: Maybe Float
    , bucketBy :: Maybe Text
    } deriving (Generic, FromJSON, ToJSON, Show, Eq)

data Segment = Segment
    { key      :: Text
    , included :: HashSet Text
    , excluded :: HashSet Text
    , salt     :: Text
    , rules    :: [SegmentRule]
    , version  :: Natural
    , deleted  :: Bool
    } deriving (Generic, FromJSON, ToJSON, Show, Eq)

data Clause = Clause
    { attribute :: Text
    , negate    :: Bool
    , op        :: Op
    , values    :: [Value]
    } deriving (Generic, FromJSON, ToJSON, Show, Eq)
