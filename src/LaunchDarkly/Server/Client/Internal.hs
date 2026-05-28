module LaunchDarkly.Server.Client.Internal
    ( Client (..)
    , Status (..)
    , clientVersion
    , makeHttpConfiguration
    , setStatus
    , getStatusI
    ) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Data.ByteString (ByteString)
import Data.Generics.Product (getField)
import Data.IORef (IORef, atomicModifyIORef', readIORef)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import GHC.Generics (Generic)
import Network.HTTP.Client (newManager)
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (HeaderName)

import LaunchDarkly.Server.Client.Status (Status (..), transitionStatus)
import LaunchDarkly.Server.Config.HttpConfiguration (HttpConfiguration (..))
import LaunchDarkly.Server.Config.Internal (ApplicationInfo, Config, getApplicationInfoHeader)
import LaunchDarkly.Server.DataSource.Internal (DataSource)
import LaunchDarkly.Server.Events (EventState)
import LaunchDarkly.Server.Store.Internal (StoreHandle, getInitializedC)

-- | The version string for this library.
clientVersion :: Text
clientVersion = "4.5.1" -- x-release-please-version

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

-- |
-- Build an 'HttpConfiguration' for the given 'Config'. A fresh TLS manager is created and a new
-- per-instance GUID v4 is generated for the 'X-LaunchDarkly-Instance-Id' header. Because the
-- returned 'defaultRequestHeaders' is shared by the polling, streaming, and event clients, every
-- outbound request carries the same stable per-instance identifier without per-channel plumbing.
--
-- The SDK key, version banner, and (optional) application tags header are also attached here.
makeHttpConfiguration :: Config -> IO HttpConfiguration
makeHttpConfiguration config = do
    tlsManager <- newManager tlsManagerSettings
    instanceId <- UUIDv4.nextRandom
    let baseHeaders =
            [ ("Authorization", encodeUtf8 $ getField @"key" config)
            , ("User-Agent", "HaskellServerClient/" <> encodeUtf8 clientVersion)
            , ("X-LaunchDarkly-Instance-Id", encodeUtf8 $ UUID.toText instanceId)
            ]
        defaultRequestHeaders = addTagsHeader baseHeaders (getField @"applicationInfo" config)
        defaultRequestTimeout = Http.responseTimeoutMicro $ fromIntegral $ getField @"requestTimeoutSeconds" config * 1000000
    pure $ HttpConfiguration {..}
  where
    addTagsHeader :: [(HeaderName, ByteString)] -> Maybe ApplicationInfo -> [(HeaderName, ByteString)]
    addTagsHeader headers Nothing = headers
    addTagsHeader headers (Just info) = case getApplicationInfoHeader info of
        Nothing -> headers
        Just header -> ("X-LaunchDarkly-Tags", encodeUtf8 header) : headers
