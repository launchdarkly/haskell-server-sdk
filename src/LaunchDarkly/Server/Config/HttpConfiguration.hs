module LaunchDarkly.Server.Config.HttpConfiguration
    ( HttpConfiguration (..)
    , prepareRequest
    )
where

import Control.Monad.Catch (MonadThrow)
import Network.HTTP.Client (Manager, Request, ResponseTimeout, parseRequest, requestHeaders, responseTimeout, setRequestIgnoreStatus)
import Network.HTTP.Types (Header)

data HttpConfiguration = HttpConfiguration
    { defaultRequestHeaders :: ![Header]
    , defaultRequestTimeout :: !ResponseTimeout
    , tlsManager :: !Manager
    }

prepareRequest :: (MonadThrow m) => HttpConfiguration -> String -> m Request
prepareRequest config uri = do
    baseReq <- parseRequest uri
    pure $
        setRequestIgnoreStatus $
            baseReq
                { requestHeaders = defaultRequestHeaders config <> requestHeaders baseReq
                , responseTimeout = defaultRequestTimeout config
                }
