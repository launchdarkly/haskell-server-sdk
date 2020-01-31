module Util.Features (makeTestFlag) where

import Data.Text                    (Text)
import GHC.Natural                  (Natural)

import LaunchDarkly.Server.Features (Flag(..), VariationOrRollout(..))

makeTestFlag :: Text -> Natural -> Flag
makeTestFlag key version = Flag
    { key                    = key
    , version                = version
    , on                     = True
    , trackEvents            = False
    , trackEventsFallthrough = False
    , deleted                = False
    , prerequisites          = []
    , salt                   = ""
    , sel                    = ""
    , targets                = []
    , rules                  = []
    , fallthrough            = VariationOrRollout
        { variation = Nothing
        , rollout   = Nothing
        }
    , offVariation           = Nothing
    , variations             = []
    , debugEventsUntilDate   = Nothing
    }
