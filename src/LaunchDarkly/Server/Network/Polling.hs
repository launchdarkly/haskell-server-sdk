module LaunchDarkly.Server.Network.Polling (pollingThread) where

import           GHC.Generics                            (Generic)
import           Data.HashMap.Strict                     (HashMap)
import           Data.Text                               (Text)
import qualified Data.Text as                            T
import           Network.HTTP.Client                     (Manager, Request(..), Response(..), httpLbs, parseRequest)
import           Data.Generics.Product                   (getField)
import           Control.Monad                           (forever)
import           Control.Concurrent                      (threadDelay)
import           Data.Aeson                              (eitherDecode, FromJSON(..))
import           Control.Monad.Logger                    (MonadLogger, logInfo, logError)
import           Control.Monad.IO.Class                  (MonadIO, liftIO)
import           Control.Monad.Catch                     (MonadMask, MonadThrow)
import           Network.HTTP.Types.Status               (ok200)

import           LaunchDarkly.Server.Client.Internal     (ClientI, Status(Initialized), setStatus)
import           LaunchDarkly.Server.Network.Common      (tryAuthorized, checkAuthorization, prepareRequest, tryHTTP)
import           LaunchDarkly.Server.Features            (Flag, Segment)
import           LaunchDarkly.Server.Store.Internal      (StoreHandle, initializeStore)

data PollingResponse = PollingResponse
    { flags    :: HashMap Text Flag
    , segments :: HashMap Text Segment
    } deriving (Generic, FromJSON, Show)

processPoll :: (MonadIO m, MonadLogger m, MonadMask m, MonadThrow m) => ClientI -> Manager -> StoreHandle IO -> Request -> m ()
processPoll client manager store request = liftIO (tryHTTP $ httpLbs request manager) >>= \case
    (Left err)       -> $(logError) (T.pack $ show err)
    (Right response) -> checkAuthorization response >> if responseStatus response /= ok200
        then $(logError) "unexpected polling status code"
        else case (eitherDecode (responseBody response) :: Either String PollingResponse) of
            (Left err)   -> $(logError) (T.pack $ show err)
            (Right body) -> do
                status <- liftIO (initializeStore store (getField @"flags" body) (getField @"segments" body))
                case status of
                    Right () -> liftIO $ setStatus client Initialized
                    Left err -> do
                        $(logError) $ T.append "store failed put: " err
                        pure ()

pollingThread :: (MonadIO m, MonadLogger m, MonadMask m) => Manager -> ClientI -> m ()
pollingThread manager client = do
    let config = getField @"config" client; store = getField @"store" client;
    req <- (liftIO $ parseRequest $ (T.unpack $ getField @"baseURI" config) ++ "/sdk/latest-all") >>= pure . prepareRequest config
    tryAuthorized client $ forever $ do
        $(logInfo) "starting poll"
        processPoll client manager store req
        $(logInfo) "finished poll"
        liftIO $ threadDelay $ (*) 1000000 $ fromIntegral $ getField @"pollIntervalSeconds" config
