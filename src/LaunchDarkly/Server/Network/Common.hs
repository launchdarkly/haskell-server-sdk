module LaunchDarkly.Server.Network.Common
    ( prepareRequest
    , withResponseGeneric
    , tryAuthorized
    , checkAuthorization
    , tryHTTP
    , addToAL
    ) where

import Data.ByteString                     (append)
import Network.HTTP.Client                 (HttpException, Manager, Request(..), Response(..), BodyReader, setRequestIgnoreStatus, responseOpen, responseTimeout, responseTimeoutMicro, responseClose)
import Network.HTTP.Types.Status           (unauthorized401, forbidden403)
import Data.Generics.Product               (getField)
import Data.Text.Encoding                  (encodeUtf8)
import Data.Function                       ((&))
import Control.Monad                       (when)
import Control.Monad.Catch                 (Exception, MonadCatch, MonadMask, MonadThrow, try, bracket, throwM)
import Control.Monad.Logger                (MonadLogger, logError)
import Control.Monad.IO.Class              (MonadIO, liftIO)

import LaunchDarkly.Server.Client.Internal (ClientI, Status(Unauthorized), clientVersion, setStatus)
import LaunchDarkly.Server.Config.Internal (ConfigI)

tryHTTP :: MonadCatch m => m a -> m (Either HttpException a)
tryHTTP = try

addToAL :: Eq k => [(k, v)] -> k -> v -> [(k, v)]
addToAL l k v = (k, v) : filter ((/=) k . fst) l

prepareRequest :: ConfigI -> Request -> Request
prepareRequest config request = request
    { requestHeaders       = (requestHeaders request)
        & \l -> addToAL l "Authorization" (encodeUtf8 $ getField @"key" config)
        & \l -> addToAL l "User-Agent" (append "HaskellSDK-" $ encodeUtf8 clientVersion)
    , responseTimeout      = responseTimeoutMicro $ (fromIntegral $ getField @"requestTimeoutSeconds" config) * 1000000
    } & setRequestIgnoreStatus

withResponseGeneric :: (MonadIO m, MonadMask m) => Request -> Manager -> (Response BodyReader -> m a) -> m a
withResponseGeneric req man f = bracket (liftIO $ responseOpen req man) (liftIO . responseClose) f

data UnauthorizedE = UnauthorizedE deriving (Show, Exception)

tryAuthorized :: (MonadIO m, MonadLogger m, MonadCatch m) => ClientI -> m a -> m ()
tryAuthorized client operation = try operation >>= \case
    (Left UnauthorizedE) -> do
        $(logError) "SDK key is unauthorized"
        liftIO $ setStatus client Unauthorized
    _                    -> pure ()

checkAuthorization :: (MonadThrow m) => Response body -> m ()
checkAuthorization response = when (elem (responseStatus response) [unauthorized401, forbidden403]) $ throwM UnauthorizedE
