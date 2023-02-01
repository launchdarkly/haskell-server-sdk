module LaunchDarkly.Server.Network.Polling (pollingThread) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logError, logInfo)
import Data.Aeson (FromJSON (..), eitherDecode)
import Data.Generics.Product (getField)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, Request (..), Response (..), httpLbs)
import Network.HTTP.Types.Status (ok200)

import LaunchDarkly.AesonCompat (KeyMap)
import LaunchDarkly.Server.Features (Flag, Segment)
import LaunchDarkly.Server.Network.Common (checkAuthorization, handleUnauthorized, tryHTTP)

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

processPoll :: (MonadIO m, MonadLogger m, MonadMask m, MonadThrow m) => Manager -> DataSourceUpdates -> Request -> m ()
processPoll manager dataSourceUpdates request =
    liftIO (tryHTTP $ httpLbs request manager) >>= \case
        (Left err) -> $(logError) (T.pack $ show err)
        (Right response) ->
            checkAuthorization response
                >> if responseStatus response /= ok200
                    then $(logError) "unexpected polling status code"
                    else case (eitherDecode (responseBody response) :: Either String PollingResponse) of
                        (Left err) -> $(logError) (T.pack $ show err)
                        (Right body) -> do
                            status <- liftIO $ dataSourceUpdatesInit dataSourceUpdates (getField @"flags" body) (getField @"segments" body)
                            case status of
                                Right () -> liftIO $ dataSourceUpdatesSetStatus dataSourceUpdates Initialized
                                Left err ->
                                    $(logError) $ T.append "store failed put: " err

pollingThread :: (MonadIO m, MonadLogger m, MonadMask m) => Text -> Natural -> ClientContext -> DataSourceUpdates -> m ()
pollingThread baseURI pollingIntervalSeconds clientContext dataSourceUpdates = do
    let pollingMicroseconds = fromIntegral pollingIntervalSeconds * 1000000
    req <- liftIO $ prepareRequest (httpConfiguration clientContext) (T.unpack baseURI ++ "/sdk/latest-all")
    handleUnauthorized dataSourceUpdates $ forever $ do
        $(logInfo) "starting poll"
        processPoll (tlsManager $ httpConfiguration clientContext) dataSourceUpdates req
        $(logInfo) "finished poll"
        liftIO $ threadDelay pollingMicroseconds
