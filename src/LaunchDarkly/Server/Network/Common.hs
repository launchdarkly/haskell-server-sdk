module LaunchDarkly.Server.Network.Common
    ( prepareRequest
    , withResponseGeneric
    , tryAuthorized
    , checkAuthorization
    , getServerTime
    , tryHTTP
    , addToAL
    , handleUnauthorized 
    ) where

import Data.ByteString                     (append)
import Data.ByteString.Internal            (unpackChars)
import Network.HTTP.Client                 (HttpException, Manager, Request(..), Response(..), BodyReader, setRequestIgnoreStatus, responseOpen, responseTimeout, responseTimeoutMicro, responseClose)
import Network.HTTP.Types.Header           (hDate)
import Network.HTTP.Types.Status           (unauthorized401, forbidden403)
import Data.Generics.Product               (getField)
import Data.Text.Encoding                  (encodeUtf8)
import Data.Time.Format                    (parseTimeM, defaultTimeLocale, rfc822DateFormat)
import Data.Time.Clock.POSIX               (utcTimeToPOSIXSeconds)
import Data.Function                       ((&))
import Data.Maybe                          (fromMaybe)
import Control.Monad                       (when)
import Control.Monad.Catch                 (Exception, MonadCatch, MonadMask, MonadThrow, try, bracket, throwM, handle)
import Control.Monad.Logger                (MonadLogger, logError)
import Control.Monad.IO.Class              (MonadIO, liftIO)

import LaunchDarkly.Server.Client.Internal     (ClientI, Status(Unauthorized), clientVersion, setStatus)
import LaunchDarkly.Server.Config.Internal     (ConfigI)
import LaunchDarkly.Server.DataSource.Internal (DataSourceUpdates(..))

tryHTTP :: MonadCatch m => m a -> m (Either HttpException a)
tryHTTP = try

addToAL :: Eq k => [(k, v)] -> k -> v -> [(k, v)]
addToAL l k v = (k, v) : filter ((/=) k . fst) l

prepareRequest :: ConfigI -> Request -> Request
prepareRequest config request = request
    { requestHeaders       = (requestHeaders request)
        & \l -> addToAL l "Authorization" (encodeUtf8 $ getField @"key" config)
        & \l -> addToAL l "User-Agent" (append "HaskellServerClient/" $ encodeUtf8 clientVersion)
    , responseTimeout      = responseTimeoutMicro $ (fromIntegral $ getField @"requestTimeoutSeconds" config) * 1000000
    } & setRequestIgnoreStatus

withResponseGeneric :: (MonadIO m, MonadMask m) => Request -> Manager -> (Response BodyReader -> m a) -> m a
withResponseGeneric req man f = bracket (liftIO $ responseOpen req man) (liftIO . responseClose) f

data UnauthorizedE = UnauthorizedE deriving (Show, Exception)

handleUnauthorized :: (MonadIO m, MonadLogger m, MonadCatch m) => DataSourceUpdates -> m () -> m ()
handleUnauthorized dataSourceUpdates = handle $ \UnauthorizedE -> do
    $(logError) "SDK key is unauthorized"
    liftIO $ dataSourceUpdatesSetStatus dataSourceUpdates Unauthorized

tryAuthorized :: (MonadIO m, MonadLogger m, MonadCatch m) => ClientI -> m a -> m ()
tryAuthorized client operation = try operation >>= \case
    (Left UnauthorizedE) -> do
        $(logError) "SDK key is unauthorized"
        liftIO $ setStatus client Unauthorized
    _                    -> pure ()

checkAuthorization :: (MonadThrow m) => Response body -> m ()
checkAuthorization response = when (elem (responseStatus response) [unauthorized401, forbidden403]) $ throwM UnauthorizedE

getServerTime :: Response body -> Int
getServerTime response
    | date == "" = 0
    | otherwise = fromMaybe 0 (truncate <$> utcTimeToPOSIXSeconds <$> parsedTime)
    where headers = responseHeaders response
          date = fromMaybe "" $ lookup hDate headers
          parsedTime = parseTimeM True defaultTimeLocale rfc822DateFormat (unpackChars date)
