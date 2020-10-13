module Util.Features (makeTestFlag, makeTestSegment) where

import Data.Text                    (Text)
import GHC.Natural                  (Natural)

import LaunchDarkly.Server.Features

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

makeTestSegment :: Text -> Natural -> Segment
makeTestSegment key version = Segment
    { key      = key
    , included = mempty
    , excluded = mempty
    , salt     = ""
    , rules    = mempty
    , version  = version
    , deleted  = False
    }
