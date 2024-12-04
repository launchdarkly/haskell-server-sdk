module LaunchDarkly.Server.Network.Eventing (eventThread) where

import qualified Codec.Compression.GZip as GZip
import Control.Concurrent (killThread, myThreadId)
import Control.Concurrent.MVar (modifyMVar_, readMVar, swapMVar, takeMVar)
import Control.Monad (forever, unless, void, when)
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebug, logError, logWarn)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as L
import Data.Function ((&))
import Data.Generics.Product (getField)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Tuple (swap)
import qualified Data.UUID as UUID
import Network.HTTP.Client (Manager, Request (..), RequestBody (..), httpLbs, responseStatus)
import Network.HTTP.Types.Status (Status (statusCode), status400)
import System.Random (newStdGen, random)
import System.Timeout (timeout)

import LaunchDarkly.Server.Client.Internal (Client, Status (ShuttingDown))
import LaunchDarkly.Server.Config.ClientContext
import LaunchDarkly.Server.Config.HttpConfiguration (prepareRequest)
import LaunchDarkly.Server.Events (EventState, processSummary)
import LaunchDarkly.Server.Network.Common (addToAL, checkAuthorization, getServerTime, isHttpUnrecoverable, tryAuthorized, tryHTTP)

-- A true result indicates a retry does not need to be attempted
processSend :: (MonadIO m, MonadLogger m, MonadMask m, MonadThrow m) => Manager -> Request -> m (Bool, Integer)
processSend manager req =
    (liftIO $ tryHTTP $ httpLbs req manager) >>= \case
        (Left err) -> $(logError) (T.pack $ show err) >> pure (False, 0)
        (Right response) -> do
            checkAuthorization response
            let code = responseStatus response
                serverTime = getServerTime response
             in $(logWarn) (T.append "@@@ server time from LD was determined to be: " $ T.pack $ show serverTime)
                    >> if code < status400
                        then pure (True, serverTime)
                        else
                            if isHttpUnrecoverable $ statusCode $ code
                                then $(logWarn) (T.append "got non recoverable event post response dropping payload: " $ T.pack $ show code) >> pure (True, serverTime)
                                else pure (False, serverTime)

setEventHeaders :: Request -> Request
setEventHeaders request =
    request
        { requestHeaders =
            (requestHeaders request)
                & \l ->
                    addToAL l "Content-Type" "application/json"
                        & \l -> addToAL l "X-LaunchDarkly-Event-Schema" "4"
        , method = "POST"
        }

updateLastKnownServerTime :: EventState -> Integer -> IO ()
updateLastKnownServerTime state serverTime = modifyMVar_ (getField @"lastKnownServerTime" state) (\lastKnown -> pure $ max serverTime lastKnown)

eventThread :: (MonadIO m, MonadLogger m, MonadMask m) => Manager -> Client -> ClientContext -> m ()
eventThread manager client clientContext = do
    let
        state = getField @"events" client
        config = getField @"config" client
        compressEvents = getField @"compressEvents" config
        httpConfig = httpConfiguration clientContext
    rngRef <- liftIO $ newStdGen >>= newIORef
    req <- (liftIO $ prepareRequest httpConfig $ (T.unpack $ getField @"eventsURI" config) ++ "/bulk") >>= pure . setEventHeaders
    void $ tryAuthorized client $ forever $ do
        liftIO $ processSummary config state
        events' <- liftIO $ swapMVar (getField @"events" state) []
        when (not $ null events') $ do
            payloadId <- liftIO $ atomicModifyIORef' rngRef (swap . random)
            let
                encoded = encode events'
                compressed = if compressEvents then GZip.compress encoded else encoded
                thisReq =
                    req
                        { requestBody = RequestBodyLBS compressed
                        , requestHeaders =
                            (requestHeaders req)
                                & \l ->
                                    addToAL l "X-LaunchDarkly-Payload-ID" (UUID.toASCIIBytes payloadId)
                                        & \l -> if compressEvents then addToAL l "Content-Encoding" "gzip" else l
                        }
            (success, serverTime) <- processSend manager thisReq
            $(logDebug) $ T.append "sending events: " $ decodeUtf8 $ L.toStrict encoded
            _ <- case success of
                True -> liftIO $ updateLastKnownServerTime state serverTime
                False -> do
                    $(logWarn) "retrying event delivery after one second"
                    liftIO $ void $ timeout (1 * 1000000) $ readMVar $ getField @"flush" state
                    (success', serverTime') <- processSend manager thisReq
                    unless success' $ do
                        $(logWarn) "failed sending events on retry, dropping event batch"
                    liftIO $ updateLastKnownServerTime state serverTime'
            $(logDebug) "finished send of event batch"
        status <- liftIO $ readIORef $ getField @"status" client
        liftIO $ when (status == ShuttingDown) (myThreadId >>= killThread)
        liftIO $ void $ timeout ((*) 1000000 $ fromIntegral $ getField @"flushIntervalSeconds" config) $ takeMVar $ getField @"flush" state
