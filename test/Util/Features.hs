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
    , contextTargets         = []
    , rules                  = []
    , fallthrough            = VariationOrRollout
        { variation = Nothing
        , rollout   = Nothing
        }
    , offVariation           = Nothing
    , variations             = []
    , debugEventsUntilDate   = Nothing
    , clientSideAvailability  = ClientSideAvailability { usingEnvironmentId = True, usingMobileKey = False, explicit = True }
    }

makeTestSegment :: Text -> Natural -> Segment
makeTestSegment key version = Segment
    { key              = key
    , included         = mempty
    , includedContexts = mempty
    , excluded         = mempty
    , excludedContexts = mempty
    , salt             = ""
    , rules            = mempty
    , version          = version
    , deleted          = False
    }
