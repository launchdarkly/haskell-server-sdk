module LaunchDarkly.Server.Config.HttpConfiguration
    ( HttpConfiguration(..)
    , prepareRequest
    )
    where

import Network.HTTP.Client (Manager, ResponseTimeout, Request, requestHeaders, responseTimeout, setRequestIgnoreStatus, parseRequest)
import Network.HTTP.Types  (Header)
import Control.Monad.Catch (MonadThrow)

data HttpConfiguration = HttpConfiguration 
    { defaultRequestHeaders :: ![Header]
    , defaultRequestTimeout :: !ResponseTimeout
    , tlsManager :: !Manager
    }

prepareRequest :: (MonadThrow m) => HttpConfiguration -> String -> m Request
prepareRequest config uri = do
    baseReq <- parseRequest uri
    pure $ setRequestIgnoreStatus $ baseReq 
        { requestHeaders       = defaultRequestHeaders config <> requestHeaders baseReq 
        , responseTimeout      = defaultRequestTimeout config 
        } 

