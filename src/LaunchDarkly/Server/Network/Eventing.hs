module LaunchDarkly.Server.Network.Eventing (eventThread) where

import           Data.Aeson                          (encode)
import           Data.Function                       ((&))
import           Data.Tuple                          (swap)
import           Data.IORef                          (newIORef, readIORef, atomicModifyIORef')
import qualified Data.UUID as                        UUID
import           Control.Monad.Logger                (MonadLogger, logDebug, logWarn, logError)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Network.HTTP.Client                 (Manager, Request(..), RequestBody(..), httpLbs, parseRequest, responseStatus)
import           Data.Generics.Product               (getField)
import qualified Data.Text as                        T
import           Control.Concurrent                  (killThread, myThreadId)
import           Control.Monad                       (forever, when, void, unless)
import           Control.Monad.Catch                 (MonadMask, MonadThrow)
import           Control.Concurrent.MVar             (takeMVar, readMVar, swapMVar)
import           System.Timeout                      (timeout)
import           System.Random                       (newStdGen, random)
import           Data.Text.Encoding                  (decodeUtf8)
import qualified Data.ByteString.Lazy as             L
import           Network.HTTP.Types.Status           (status400, status408, status429, status500)

import           LaunchDarkly.Server.Client.Internal (ClientI, Status(ShuttingDown))
import           LaunchDarkly.Server.Network.Common  (tryAuthorized, checkAuthorization, prepareRequest, tryHTTP, addToAL)
import           LaunchDarkly.Server.Events          (processSummary)

-- A true result indicates a retry does not need to be attempted
processSend :: (MonadIO m, MonadLogger m, MonadMask m, MonadThrow m) => Manager -> Request -> m Bool
processSend manager req = (liftIO $ tryHTTP $ httpLbs req manager) >>= \case
    (Left err)       -> $(logError) (T.pack $ show err) >> pure False
    (Right response) -> checkAuthorization response     >> let code = responseStatus response in
        if code < status400 then pure True else if (elem code [status400, status408, status429]) || code >= status500 then pure False else
            $(logWarn) (T.append "got non recoverable event post response dropping payload: " $ T.pack $ show code) >> pure True

setEventHeaders :: Request -> Request
setEventHeaders request = request
    { requestHeaders = (requestHeaders request)
        & \l -> addToAL l "Content-Type" "application/json"
        & \l -> addToAL l "X-LaunchDarkly-Event-Schema" "3"
    , method         = "POST"
    }

eventThread :: (MonadIO m, MonadLogger m, MonadMask m) => Manager -> ClientI -> m ()
eventThread manager client = do
    let state = getField @"events" client; config = getField @"config" client;
    rngRef <- liftIO $ newStdGen >>= newIORef
    req <- (liftIO $ parseRequest $ (T.unpack $ getField @"eventsURI" config) ++ "/bulk") >>= pure . setEventHeaders . prepareRequest config
    void $ tryAuthorized client $ forever $ do
        liftIO $ processSummary config state
        events' <- liftIO $ swapMVar (getField @"events" state) []
        when (not $ null events') $ do
            payloadId <- liftIO $ atomicModifyIORef' rngRef (swap . random)
            let
                encoded = encode events'
                thisReq = req
                    { requestBody    = RequestBodyLBS encoded
                    , requestHeaders = (requestHeaders req)
                        & \l -> addToAL l "X-LaunchDarkly-Payload-ID" (UUID.toASCIIBytes payloadId)
                    }
            $(logDebug) $ T.append "sending events: " $ decodeUtf8 $ L.toStrict encoded
            success <- processSend manager thisReq
            unless success $ do
                $(logWarn) "retrying event delivery after one second"
                liftIO $ void $ timeout (1 * 1000000) $ readMVar $ getField @"flush" state
                success' <- processSend manager thisReq
                unless success' $ $(logWarn) "failed sending events on retry, dropping event batch"
            $(logDebug) "finished send of event batch"
        status <- liftIO $ readIORef $ getField @"status" client
        liftIO $ when (status == ShuttingDown) (myThreadId >>= killThread)
        liftIO $ void $ timeout ((*) 1000000 $ fromIntegral $ getField @"flushIntervalSeconds" config) $ takeMVar $ getField @"flush" state
