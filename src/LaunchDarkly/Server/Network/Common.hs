module LaunchDarkly.Server.Network.Common
    ( withResponseGeneric
    , tryAuthorized
    , checkAuthorization
    , throwIfNot200
    , getServerTime
    , tryHTTP
    , addToAL
    , handleUnauthorized
    , isHttpUnrecoverable
    ) where

import Control.Monad (when)
import Control.Monad.Catch (Exception, MonadCatch, MonadMask, MonadThrow, bracket, handle, throwM, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logError)
import Data.ByteString.Internal (unpackChars)
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, parseTimeM, rfc822DateFormat)
import Network.HTTP.Client (BodyReader, HttpException, Manager, Request (..), Response (..), responseClose, responseOpen, throwErrorStatusCodes)
import Network.HTTP.Types.Header (hDate)
import Network.HTTP.Types.Status (forbidden403, unauthorized401)

import LaunchDarkly.Server.Client.Internal (Client, Status (Unauthorized), setStatus)
import LaunchDarkly.Server.DataSource.Internal (DataSourceUpdates (..))
import Network.HTTP.Types (ok200)

tryHTTP :: MonadCatch m => m a -> m (Either HttpException a)
tryHTTP = try

addToAL :: Eq k => [(k, v)] -> k -> v -> [(k, v)]
addToAL l k v = (k, v) : filter ((/=) k . fst) l

withResponseGeneric :: (MonadIO m, MonadMask m) => Request -> Manager -> (Response BodyReader -> m a) -> m a
withResponseGeneric req man f = bracket (liftIO $ responseOpen req man) (liftIO . responseClose) f

data UnauthorizedE = UnauthorizedE deriving (Show, Exception)

handleUnauthorized :: (MonadIO m, MonadLogger m, MonadCatch m) => DataSourceUpdates -> m () -> m ()
handleUnauthorized dataSourceUpdates = handle $ \UnauthorizedE -> do
    $(logError) "SDK key is unauthorized"
    liftIO $ dataSourceUpdatesSetStatus dataSourceUpdates Unauthorized

tryAuthorized :: (MonadIO m, MonadLogger m, MonadCatch m) => Client -> m a -> m ()
tryAuthorized client operation =
    try operation >>= \case
        (Left UnauthorizedE) -> do
            $(logError) "SDK key is unauthorized"
            liftIO $ setStatus client Unauthorized
        _ -> pure ()

checkAuthorization :: (MonadThrow m) => Response body -> m ()
checkAuthorization response = when (elem (responseStatus response) [unauthorized401, forbidden403]) $ throwM UnauthorizedE

throwIfNot200 :: (MonadIO m) => Request -> Response BodyReader -> m ()
throwIfNot200 request response = when (responseStatus response /= ok200) $ throwErrorStatusCodes request response

getServerTime :: Response body -> Integer
getServerTime response
    | date == "" = 0
    | otherwise = fromMaybe 0 (truncate <$> utcTimeToPOSIXSeconds <$> parsedTime)
  where
    headers = responseHeaders response
    date = fromMaybe "" $ lookup hDate headers
    parsedTime = parseTimeM True defaultTimeLocale rfc822DateFormat (unpackChars date)

isHttpUnrecoverable :: Int -> Bool
isHttpUnrecoverable status
    | status < 400 || status >= 500 = False
    | status `elem` [400, 408, 429] = False
    | otherwise = True
