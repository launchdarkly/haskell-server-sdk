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

import LaunchDarkly.Server.Config.Internal (ConfigI)
import LaunchDarkly.Server.Store.Internal  (StoreHandle, getInitializedC)
import LaunchDarkly.Server.Events          (EventState)

-- | Client is the LaunchDarkly client. Client instances are thread-safe.
-- Applications should instantiate a single instance for the lifetime of their
-- application.
newtype Client = Client ClientI

-- | The version string for this library.
clientVersion :: Text
clientVersion = "2.0.0"

-- | The status of the client initialization.
data Status
    = Uninitialized
      -- ^ The client has not yet finished connecting to LaunchDarkly.
    | Unauthorized
      -- ^ The client attempted to connect to LaunchDarkly and was denied.
    | Initialized
      -- ^ The client has successfuly connected to LaunchDarkly.
    | ShuttingDown
      -- ^ The client is being terminated
    deriving (Eq)

setStatus :: ClientI -> Status -> IO ()
setStatus client status' = atomicModifyIORef' (getField @"status" client) $ \status ->
    case status' of
        -- Only allow setting Initialized if Uninitialized
        Initialized   -> (if status == Uninitialized then Initialized  else status, ())
        -- Only allow setting status if not ShuttingDown
        _             -> (if status == ShuttingDown  then ShuttingDown else status', ())

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
    , downloadThreadPair :: !(Maybe (ThreadId, MVar ()))
    , eventThreadPair    :: !(Maybe (ThreadId, MVar ()))
    } deriving (Generic)
