module LaunchDarkly.Server.Client.Internal where

import Data.Text                  (Text)
import Data.IORef                 (IORef)
import GHC.Generics               (Generic)

import LaunchDarkly.Server.Config (Config)
import LaunchDarkly.Server.Store  (StoreHandle)
import LaunchDarkly.Server.Events (EventState)

-- | The version string for this library.
clientVersion :: Text
clientVersion = "1.0.0-beta.1"

-- | The status of the client initialization.
data Status
    = Uninitialized
      -- ^ The client has not yet finished connecting to LaunchDarkly.
    | Unauthorized
      -- ^ The client attempted to connect to LaunchDarkly and was denied.
    | Initialized
      -- ^ The client has successfuly connected to LaunchDarkly.

-- | Client is the LaunchDarkly client. Client instances are thread-safe.
-- Applications should instantiate a single instance for the lifetime of their
-- application.
data Client = Client
    { config :: Config
    , store  :: StoreHandle IO
    , status :: IORef Status
    , events :: EventState
    } deriving (Generic)
