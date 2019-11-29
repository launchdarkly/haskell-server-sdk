module LaunchDarkly.Server.Network.Common
    ( prepareRequest
    , withResponseGeneric
    , tryAuthorized
    , checkAuthorization
    , tryHTTP
    , addToAL
    ) where

import Data.ByteString            (append)
import Network.HTTP.Client        (HttpException, Manager, Request(..), Response(..), BodyReader, setRequestIgnoreStatus, responseOpen, responseClose)
import Network.HTTP.Types.Status  (unauthorized401, forbidden403)
import Data.Generics.Product      (getField)
import Data.Text.Encoding         (encodeUtf8)
import Data.Function              ((&))
import Data.IORef                 (writeIORef)
import Control.Monad              (when)
import Control.Monad.Catch        (Exception, MonadCatch, MonadMask, MonadThrow, try, bracket, throwM)
import Control.Monad.IO.Class     (MonadIO, liftIO)

import LaunchDarkly.Server.Client (Client, Status(Unauthorized), clientVersion)
import LaunchDarkly.Server.Config (Config)

tryHTTP :: MonadCatch m => m a -> m (Either HttpException a)
tryHTTP = try

addToAL :: Eq k => [(k, v)] -> k -> v -> [(k, v)]
addToAL l k v = (k, v) : filter ((/=) k . fst) l

prepareRequest :: Config -> Request -> Request
prepareRequest config request = request
    { requestHeaders = (requestHeaders request)
        & \l -> addToAL l "Authorization" (encodeUtf8 $ getField @"key" config)
        & \l -> addToAL l "User-Agent" (append "HaskellSDK-" $ encodeUtf8 clientVersion)
    } & setRequestIgnoreStatus

withResponseGeneric :: (MonadIO m, MonadMask m) => Request -> Manager -> (Response BodyReader -> m a) -> m a
withResponseGeneric req man f = bracket (liftIO $ responseOpen req man) (liftIO . responseClose) f

data UnauthorizedE = UnauthorizedE deriving (Show, Exception)

tryAuthorized :: (MonadIO m, MonadCatch m) => Client -> m a -> m ()
tryAuthorized client operation = try operation >>= \case
    (Left UnauthorizedE) -> liftIO $ writeIORef (getField @"status" client) Unauthorized
    _                    -> pure ()

checkAuthorization :: (MonadThrow m) => Response body -> m ()
checkAuthorization response = when (elem (responseStatus response) [unauthorized401, forbidden403]) $ throwM UnauthorizedE
