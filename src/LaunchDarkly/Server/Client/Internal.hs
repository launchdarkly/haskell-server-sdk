module LaunchDarkly.Server.Client.Internal
    ( Client (..)
    , Status (..)
    , clientVersion
    , setStatus
    , getStatusI
    ) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Data.Generics.Product (getField)
import Data.IORef (IORef, atomicModifyIORef', readIORef)
import Data.Text (Text)
import GHC.Generics (Generic)

import LaunchDarkly.Server.Client.Status (Status (..), transitionStatus)
import LaunchDarkly.Server.Config.Internal (Config)
import LaunchDarkly.Server.DataSource.Internal (DataSource)
import LaunchDarkly.Server.Events (EventState)
import LaunchDarkly.Server.Store.Internal (StoreHandle, getInitializedC)

-- | The version string for this library.
clientVersion :: Text
clientVersion = "4.3.0" -- x-release-please-version

-- |
-- Client is the LaunchDarkly client. Client instances are thread-safe.
-- Applications should instantiate a single instance for the lifetime of their
-- application.
data Client = Client
    { config :: !(Config)
    , store :: !(StoreHandle IO)
    , status :: !(IORef Status)
    , events :: !EventState
    , eventThreadPair :: !(Maybe (ThreadId, MVar ()))
    , dataSource :: !DataSource
    }
    deriving (Generic)

setStatus :: Client -> Status -> IO ()
setStatus client status' =
    atomicModifyIORef' (getField @"status" client) (fmap (,()) (transitionStatus status'))

getStatusI :: Client -> IO Status
getStatusI client =
    readIORef (getField @"status" client) >>= \case
        Unauthorized -> pure Unauthorized
        ShuttingDown -> pure ShuttingDown
        _ ->
            getInitializedC (getField @"store" client) >>= \case
                Right True -> pure Initialized
                _ -> pure Uninitialized
