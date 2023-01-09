module LaunchDarkly.Server.Client.Internal
    ( Client(..)
    , ClientI(..)
    , Status(..)
    , clientVersion
    , setStatus
    , getStatusI
    ) where

import Data.Text                           (Text)
import Data.IORef                          (IORef, readIORef, atomicModifyIORef')
import GHC.Generics                        (Generic)
import Control.Concurrent                  (ThreadId)
import Control.Concurrent.MVar             (MVar)
import Data.Generics.Product               (getField)

import LaunchDarkly.Server.Client.Status       (Status(..), transitionStatus)
import LaunchDarkly.Server.Config.Internal     (ConfigI)
import LaunchDarkly.Server.Store.Internal      (StoreHandle, getInitializedC)
import LaunchDarkly.Server.Events              (EventState)
import LaunchDarkly.Server.DataSource.Internal (DataSource)

-- | Client is the LaunchDarkly client. Client instances are thread-safe.
-- Applications should instantiate a single instance for the lifetime of their
-- application.
newtype Client = Client ClientI

-- | The version string for this library.
clientVersion :: Text
clientVersion = "3.0.4"

setStatus :: ClientI -> Status -> IO ()
setStatus client status' =
    atomicModifyIORef' (getField @"status" client) (fmap (,()) (transitionStatus status'))

getStatusI :: ClientI -> IO Status
getStatusI client = readIORef (getField @"status" client) >>= \case
    Unauthorized -> pure Unauthorized
    ShuttingDown -> pure ShuttingDown
    _            -> getInitializedC (getField @"store" client) >>= \case
        Right True -> pure Initialized
        _          -> pure Uninitialized

data ClientI = ClientI
    { config             :: !(ConfigI)
    , store              :: !(StoreHandle IO)
    , status             :: !(IORef Status)
    , events             :: !EventState
    , eventThreadPair    :: !(Maybe (ThreadId, MVar ()))
    , dataSource         :: !DataSource
    } deriving (Generic)
