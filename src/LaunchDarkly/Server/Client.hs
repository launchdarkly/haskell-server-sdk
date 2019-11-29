module LaunchDarkly.Server.Client where

import Data.Text                  (Text)
import Data.IORef                 (IORef)
import GHC.Generics               (Generic)

import LaunchDarkly.Server.Config (Config)
import LaunchDarkly.Server.Store  (StoreHandle)
import LaunchDarkly.Server.Events (EventState)

clientVersion :: Text
clientVersion = "1.0.0-beta.1"

data Status = Uninitialized | Unauthorized | Initialized

data Client = Client
    { config :: Config
    , store  :: StoreHandle IO
    , status :: IORef Status
    , events :: EventState
    } deriving (Generic)
