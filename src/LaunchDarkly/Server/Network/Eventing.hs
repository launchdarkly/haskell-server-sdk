module LaunchDarkly.Server.Network.Eventing (eventThread) where

import           Data.Aeson                          (encode)
import           Data.Function                       ((&))
import           Data.IORef                          (readIORef)
import           Control.Monad.Logger                (MonadLogger, logInfo, logError)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Network.HTTP.Client                 (Manager, Request(..), RequestBody(..), httpLbs, parseRequest)
import           Data.Generics.Product               (getField)
import qualified Data.Text as                        T
import           Control.Concurrent                  (killThread, myThreadId)
import           Control.Monad                       (forever, when, void)
import           Control.Monad.Catch                 (MonadMask, MonadThrow)
import           Control.Concurrent.MVar             (takeMVar, swapMVar)
import           System.Timeout                      (timeout)
import           Data.Text.Encoding                  (decodeUtf8)
import qualified Data.ByteString.Lazy as             L

import           LaunchDarkly.Server.Client.Internal (ClientI, Status(ShuttingDown))
import           LaunchDarkly.Server.Network.Common  (tryAuthorized, checkAuthorization, prepareRequest, tryHTTP, addToAL)
import           LaunchDarkly.Server.Events          (processSummary)

processSend :: (MonadIO m, MonadLogger m, MonadMask m, MonadThrow m) => Manager -> Request -> m ()
processSend manager req = (liftIO $ tryHTTP $ httpLbs req manager) >>= \case
    (Left err)       -> $(logError) (T.pack $ show err)
    (Right response) -> checkAuthorization response

setEventHeaders :: Request -> Request
setEventHeaders request = request
    { requestHeaders = (requestHeaders request)
        & \l -> addToAL l "Content-Type" "application/json"
        & \l -> addToAL l "X-LaunchDarkly-Event-Schema" "3"
    }

eventThread :: (MonadIO m, MonadLogger m, MonadMask m) => Manager -> ClientI -> m ()
eventThread manager client = do
    let state = getField @"events" client; config = getField @"config" client;
    req <- (liftIO $ parseRequest $ (T.unpack $ getField @"eventsURI" config) ++ "/bulk") >>= pure . setEventHeaders . prepareRequest config
    void $ tryAuthorized client $ forever $ do
        liftIO $ processSummary config state
        events' <- liftIO $ swapMVar (getField @"events" state) []
        when (not $ null events') $ do
            let encoded = encode events'
            $(logInfo) $ T.append "sending events: " $ decodeUtf8 $ L.toStrict encoded
            let thisReq = req { method = "POST", requestBody = RequestBodyLBS encoded }
            $(logInfo) "starting send of event batch"
            processSend manager thisReq
            $(logInfo) "finished send of event batch"
        status <- liftIO $ readIORef $ getField @"status" client
        liftIO $ when (status == ShuttingDown) (myThreadId >>= killThread)
        liftIO $ void $ timeout ((*) 1000000 $ fromIntegral $ getField @"flushIntervalSeconds" config) $ takeMVar $ getField @"flush" state
