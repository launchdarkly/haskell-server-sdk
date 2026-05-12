module LaunchDarkly.Server.Config.HttpConfiguration
    ( HttpConfiguration (..)
    , prepareRequest
    , instanceIdHeader
    , makeInstanceIdHeader
    )
where

import Control.Monad.Catch (MonadThrow)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import Network.HTTP.Client (Manager, Request, ResponseTimeout, parseRequest, requestHeaders, responseTimeout, setRequestIgnoreStatus)
import Network.HTTP.Types (Header, HeaderName)

data HttpConfiguration = HttpConfiguration
    { defaultRequestHeaders :: ![Header]
    , defaultRequestTimeout :: !ResponseTimeout
    , tlsManager :: !Manager
    }

-- |
-- The HTTP header used to identify this SDK instance for the purpose of estimating
-- server-connection-minutes when polling. It contains a v4 UUID that is generated once per SDK
-- instance and remains constant for the lifetime of the client.
--
-- See: sdk-specs / SCMP-server-connection-minutes-polling.
instanceIdHeader :: HeaderName
instanceIdHeader = "X-LaunchDarkly-Instance-Id"

-- |
-- Generate a fresh 'X-LaunchDarkly-Instance-Id' header carrying a new v4 UUID.
--
-- The caller is expected to invoke this exactly once per SDK instance and store the result on
-- 'defaultRequestHeaders' so that the same identifier rides every polling, streaming, and event
-- request issued by that instance.
makeInstanceIdHeader :: IO Header
makeInstanceIdHeader = do
    instanceId <- UUIDv4.nextRandom
    pure (instanceIdHeader, encodeUtf8 $ UUID.toText instanceId)

prepareRequest :: (MonadThrow m) => HttpConfiguration -> String -> m Request
prepareRequest config uri = do
    baseReq <- parseRequest uri
    pure $
        setRequestIgnoreStatus $
            baseReq
                { requestHeaders = defaultRequestHeaders config <> requestHeaders baseReq
                , responseTimeout = defaultRequestTimeout config
                }
