module LaunchDarkly.Server.Network.Polling (pollingThread) where

import Control.Concurrent (threadDelay)
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logError, logInfo)
import Data.Aeson (FromJSON (..), eitherDecode)
import Data.Generics.Product (getField)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, Request (..), Response (..), httpLbs)
import Network.HTTP.Types.Status (Status (statusCode), ok200)

import LaunchDarkly.AesonCompat (KeyMap)
import LaunchDarkly.Server.Features (Flag, Segment)
import LaunchDarkly.Server.Network.Common (checkAuthorization, handleUnauthorized, isHttpUnrecoverable, tryHTTP)

import Data.ByteString.Lazy (ByteString)
import GHC.Natural (Natural)
import LaunchDarkly.Server.Client.Internal (Status (..))
import LaunchDarkly.Server.Config.ClientContext
import LaunchDarkly.Server.Config.HttpConfiguration (HttpConfiguration (..), prepareRequest)
import LaunchDarkly.Server.DataSource.Internal (DataSourceUpdates (..))

data PollingResponse = PollingResponse
    { flags :: !(KeyMap Flag)
    , segments :: !(KeyMap Segment)
    }
    deriving (Generic, FromJSON, Show)

processPoll :: (MonadIO m, MonadLogger m, MonadMask m, MonadThrow m) => Manager -> DataSourceUpdates -> Request -> m Bool
processPoll manager dataSourceUpdates request =
    liftIO (tryHTTP $ httpLbs request manager) >>= \case
        (Left err) -> do
            $(logError) (T.pack $ show err)
            pure True
        (Right response) ->
            checkAuthorization response >> processResponse response
  where
    processResponse :: (MonadIO m, MonadLogger m, MonadMask m, MonadThrow m) => Response ByteString -> m Bool
    processResponse response
        | isHttpUnrecoverable $ statusCode $ responseStatus response = do
            $(logError) "polling stopping after receiving unrecoverable error"
            pure False
        | responseStatus response /= ok200 = do
            $(logError) "unexpected polling status code"
            pure True
        | otherwise = case (eitherDecode (responseBody response) :: Either String PollingResponse) of
            (Left err) -> do
                $(logError) (T.pack $ show err)
                pure $ True
            (Right body) -> do
                status <- liftIO $ dataSourceUpdatesInit dataSourceUpdates (getField @"flags" body) (getField @"segments" body)
                case status of
                    Right () -> do
                        liftIO $ dataSourceUpdatesSetStatus dataSourceUpdates Initialized
                        pure $ True
                    Left err -> do
                        $(logError) $ T.append "store failed put: " err
                        pure $ True

pollingThread :: (MonadIO m, MonadLogger m, MonadMask m) => Text -> Natural -> ClientContext -> DataSourceUpdates -> m ()
pollingThread baseURI pollingIntervalSeconds clientContext dataSourceUpdates = do
    let pollingMicroseconds = fromIntegral pollingIntervalSeconds * 1000000
    req <- liftIO $ prepareRequest (httpConfiguration clientContext) (T.unpack baseURI ++ "/sdk/latest-all")
    handleUnauthorized dataSourceUpdates $ (poll req pollingMicroseconds)
  where
    poll :: (MonadIO m, MonadLogger m, MonadMask m) => Request -> Int -> m ()
    poll req pollingMicroseconds = do
        $(logInfo) "starting poll"
        processPoll (tlsManager $ httpConfiguration clientContext) dataSourceUpdates req >>= \case
            True -> do
                liftIO $ threadDelay pollingMicroseconds
                poll req pollingMicroseconds
            False -> pure ()
