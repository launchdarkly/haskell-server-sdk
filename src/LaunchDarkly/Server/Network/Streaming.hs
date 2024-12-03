{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module LaunchDarkly.Server.Network.Streaming (streamingThread) where

import Control.Applicative (many)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (mzero, void)
import Control.Monad.Catch (Exception, MonadCatch, MonadMask, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebug, logError, logWarn)
import Control.Monad.Loops (iterateUntilM)
import Data.Aeson (FromJSON, Result (..), eitherDecode, fromJSON, parseJSON, withObject, (.!=), (.:), (.:?))
import Data.Attoparsec.Text as P hiding (Result, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), Manager, Request, Response (..), brRead)
import Network.HTTP.Types.Status (Status (statusCode))
import System.Clock (Clock (Monotonic), TimeSpec (TimeSpec), getTime)
import System.Random (Random (randomR), newStdGen)
import System.Timeout (timeout)

import LaunchDarkly.AesonCompat (KeyMap, emptyObject)
import LaunchDarkly.Server.Config.ClientContext (ClientContext (..))
import LaunchDarkly.Server.Config.HttpConfiguration (HttpConfiguration (..), prepareRequest)
import LaunchDarkly.Server.DataSource.Internal (DataSourceUpdates (..))
import LaunchDarkly.Server.Features (Flag, Segment)
import LaunchDarkly.Server.Network.Common (checkAuthorization, handleUnauthorized, isHttpUnrecoverable, throwIfNot200, tryHTTP, withResponseGeneric)
import LaunchDarkly.Server.Store.Internal (StoreResult)

data PutBody = PutBody
    { flags :: !(KeyMap Flag)
    , segments :: !(KeyMap Segment)
    }
    deriving (Generic, Show)

data PathData d = PathData
    { path :: !Text
    , pathData :: !d
    }
    deriving (Generic, Show)

data PathVersion = PathVersion
    { path :: !Text
    , version :: !Natural
    }
    deriving (Generic, Show, FromJSON)

instance FromJSON PutBody where
    parseJSON = withObject "PutBody" $ \o -> do
        flags <- o .: "flags"
        segments <- o .:? "segments" .!= emptyObject
        pure $ PutBody {flags = flags, segments = segments}

instance FromJSON a => FromJSON (PathData a) where
    parseJSON = withObject "Put" $ \o -> do
        pathData <- o .: "data"
        path <- o .:? "path" .!= "/"
        pure $ PathData {path = path, pathData = pathData}

data SSE = SSE
    { name :: !Text
    , buffer :: !Text
    , lastEventId :: !(Maybe Text)
    , retry :: !(Maybe Text)
    }
    deriving (Generic, Show, Eq)

nameCharPredicate :: Char -> Bool
nameCharPredicate x = x /= '\r' && x /= ':' && x /= '\n'

anyCharPredicate :: Char -> Bool
anyCharPredicate x = x /= '\r' && x /= '\n'

endOfLineSSE :: Parser ()
endOfLineSSE = choice [void $ string "\r\n", void $ string "\r", void $ string "\n", endOfInput]

comment :: Parser ()
comment = char ':' >> P.takeWhile anyCharPredicate >> endOfLineSSE >> pure ()

parseField :: Parser (Text, Text)
parseField = do
    fieldName <- P.takeWhile1 nameCharPredicate
    void $ option ' ' $ char ':'
    void $ option ' ' $ char ' '
    fieldValue <- P.takeWhile anyCharPredicate
    endOfLineSSE
    pure (fieldName, fieldValue)

processField :: (Text, Text) -> SSE -> SSE
processField (fieldName, fieldValue) event = case fieldName of
    "event" -> event {name = fieldValue}
    "id" -> event {lastEventId = Just fieldValue}
    "retry" -> event {retry = Just fieldValue}
    "data" -> event {buffer = T.concat [buffer event, if T.null (buffer event) then "" else "\n", fieldValue]}
    _ -> event

parseEvent :: Parser SSE
parseEvent = do
    fields <- many (many comment >> parseField >>= pure)
    endOfLineSSE
    let event = foldr processField (SSE "" "" mzero mzero) fields
    if T.null (name event) || T.null (buffer event) then parseEvent else pure event

processPut :: (MonadIO m, MonadLogger m) => DataSourceUpdates -> L.ByteString -> m Bool
processPut dataSourceUpdates value = case eitherDecode value of
    Right (PathData _ (PutBody flags segments)) -> do
        $(logDebug) "initializing dataSourceUpdates with put"
        liftIO (dataSourceUpdatesInit dataSourceUpdates flags segments) >>= \case
            Left err -> do
                $(logError) $ T.append "dataSourceUpdates failed put: " err
                pure False
            _ -> pure True
    Left err -> do
        $(logError) $ T.append "failed to parse put body" (T.pack err)
        pure False

processPatch :: forall m. (MonadIO m, MonadLogger m) => DataSourceUpdates -> L.ByteString -> m Bool
processPatch dataSourceUpdates value = case eitherDecode value of
    Right (PathData path body)
        | T.isPrefixOf "/flags/" path -> insPatch "flag" path dataSourceUpdatesInsertFlag (fromJSON body)
        | T.isPrefixOf "/segments/" path -> insPatch "segment" path dataSourceUpdatesInsertSegment (fromJSON body)
        | otherwise -> do
            $(logError) "unknown patch path"
            pure True
    Left err -> do
        $(logError) $ T.append "failed to parse patch generic" (T.pack err)
        pure False
  where
    insPatch :: Text -> Text -> (DataSourceUpdates -> a -> IO (Either Text ())) -> Result a -> m Bool
    insPatch name _ _ (Error err) = do
        $(logError) $ T.concat ["failed to parse patch ", name, ": ", T.pack err]
        pure False
    insPatch name path insert (Success item) = do
        $(logDebug) $ T.concat ["patching ", name, " with path: ", path]
        status <- liftIO $ insert dataSourceUpdates item
        either
            ( \err -> do
                $(logError) $ T.concat ["dataSourceUpdates failed ", name, " patch: ", err]
                pure False
            )
            (const $ pure True)
            status

processDelete :: forall m. (MonadIO m, MonadLogger m) => DataSourceUpdates -> L.ByteString -> m Bool
processDelete dataSourceUpdates value = case eitherDecode value :: Either String PathVersion of
    Right (PathVersion path version)
        | T.isPrefixOf "/flags/" path -> logDelete "flag" path (dataSourceUpdatesDeleteFlag dataSourceUpdates (T.drop 7 path) version)
        | T.isPrefixOf "/segments/" path -> logDelete "segment" path (dataSourceUpdatesDeleteSegment dataSourceUpdates (T.drop 10 path) version)
        | otherwise -> do
            $(logError) "unknown delete path"
            pure False
    Left err -> do
        $(logError) $ T.append "failed to parse delete" (T.pack err)
        pure False
  where
    logDelete :: Text -> Text -> StoreResult () -> m Bool
    logDelete name path action = do
        $(logDebug) $ T.concat ["deleting ", name, " with path: ", path]
        status <- liftIO action
        either
            ( \err -> do
                $(logError) $ T.concat ["dataSourceUpdates failed ", name, " delete: ", err]
                pure False
            )
            (const $ pure True)
            status

processEvent :: (MonadIO m, MonadLogger m) => DataSourceUpdates -> Text -> L.ByteString -> m Bool
processEvent dataSourceUpdates name value = case name of
    "put" -> processPut dataSourceUpdates value
    "patch" -> processPatch dataSourceUpdates value
    "delete" -> processDelete dataSourceUpdates value
    _ -> do
        $(logWarn) "unknown event type"
        pure True

data ReadE = ReadETimeout | ReadEClosed deriving (Show, Exception)

tryReadE :: MonadCatch m => m a -> m (Either ReadE a)
tryReadE = try

-- heartbeat expected every 120 seconds
readWithException :: IO ByteString -> IO Text
readWithException body =
    timeout (1_000_000 * 300) (brRead body) >>= \case
        Nothing -> throwIO ReadETimeout
        Just bytes -> if bytes == B.empty then throwIO ReadEClosed else pure (decodeUtf8 bytes)

readStream :: (MonadIO m, MonadLogger m, MonadMask m) => IO ByteString -> DataSourceUpdates -> m Bool
readStream body dataSourceUpdates = loop "" False
  where
    loop initial processedEvent =
        tryReadE (parseWith (liftIO $ readWithException body) parseEvent initial) >>= \case
            (Left ReadETimeout) -> do
                $(logError) "streaming connection unexpectedly closed"
                pure processedEvent
            (Left ReadEClosed) -> do
                $(logError) "timeout waiting for SSE event"
                pure processedEvent
            (Right parsed) -> case parsed of
                Done remaining event -> do
                    processed <- processEvent dataSourceUpdates (name event) (L.fromStrict $ encodeUtf8 $ buffer event)
                    if processed then loop remaining True else pure processedEvent
                Fail _ context err -> do
                    $(logError) $ T.intercalate " " ["failed parsing SSE frame", T.pack $ show context, T.pack err]
                    pure processedEvent
                Partial _ -> do
                    $(logError) "failed parsing SSE frame unexpected partial"
                    pure processedEvent

-- This function is responsible for consuming a streaming connection.
--
-- It is responsible for opening the connection, parsing the body for as long as it is able, and then handling shut down
-- cleanly. While events are being processed, it is responsible for implementing a sane retry policy that prevents the
-- stampeding herd problem in the unlikely event of upstream failures.
startNewConnection :: (MonadIO m, MonadLogger m, MonadMask m) => Manager -> Request -> DataSourceUpdates -> StreamingState -> m StreamingState
startNewConnection manager request dataSourceUpdates state@(StreamingState {activeSince}) = do
    $(logDebug) "starting new streaming connection"
    status <- tryHTTP $ withResponseGeneric request manager $ \response -> do
        checkAuthorization response
            >> throwIfNot200 request response
            >> readStream (responseBody response) dataSourceUpdates
    now <- liftIO $ getTime Monotonic
    handleResponse state now status
  where
    -- This function is responsible for parsing the final output from a now closed streaming connection.
    --
    -- Given the result of the stream run, this function can choose to either mark the state as cancelled, stopping all
    -- further attempts, or it can update the state and wait some amount of time before starting another attempt.
    handleResponse :: (MonadIO m, MonadLogger m, MonadMask m) => StreamingState -> TimeSpec -> (Either HttpException Bool) -> m StreamingState
    handleResponse state now result =
        let state' = updateState state now result
         in if cancel state'
                then pure state'
                else do
                    delay <- calculateDelay state'
                    _ <- liftIO $ threadDelay delay
                    pure state'

    -- Once a streaming connection run has ended, we need to update the streaming state.
    --
    -- Given the result of a stream run, this function can either choose to mark the state as cancelled, meaning halt all
    -- further attempts, or it can be updated according to the rules of our streaming specification.
    updateState :: StreamingState -> TimeSpec -> (Either HttpException Bool) -> StreamingState
    updateState state now (Right _) = state {initialConnection = False, activeSince = Just now, attempt = 1}
    updateState state@(StreamingState {attempt = att}) now (Left (HttpExceptionRequest _ (StatusCodeException response _)))
        | isHttpUnrecoverable code = state {cancel = True}
        | otherwise = do
            case activeSince of
                (Just time)
                    | (now >= time + (TimeSpec 60 0)) -> state {attempt = 1, activeSince = Nothing}
                    | otherwise -> state {attempt = att + 1}
                Nothing -> state {attempt = att + 1}
      where
        code = statusCode (responseStatus response)
    updateState state@(StreamingState {attempt = att}) _ _ = state {attempt = att + 1}

    -- Calculate the next delay period following a backoff + jitter + max delay algorithm.
    --
    -- See https://aws.amazon.com/blogs/architecture/exponential-backoff-and-jitter/
    calculateDelay :: (MonadIO m, MonadLogger m, MonadMask m) => StreamingState -> m Int
    calculateDelay StreamingState {initialRetryDelay, attempt = att} = do
        liftIO $
            newStdGen >>= \gen ->
                let timespan = min (30 * 1_000_000) ((initialRetryDelay * 1_000) * (2 ^ (att - 1)))
                    jitter = fst $ randomR (0, timespan `div` 2) gen
                 in pure $ (timespan - jitter)

data StreamingState = StreamingState
    { initialConnection :: Bool -- Marker used to determine the first time the streamer connects.
    , initialRetryDelay :: Int -- The base duration used for the retry delay calculation
    , activeSince :: Maybe TimeSpec -- TimeSpec to denote the last time the SDK successfully connected to the stream
    , attempt :: Int -- A number representing the attempt # of this connection
    , cancel :: Bool -- A marker to shortcut logic and halt the streaming process
    }
    deriving (Generic, Show)

-- Start a thread for streaming events back from LaunchDarkly services.
streamingThread :: (MonadIO m, MonadLogger m, MonadMask m) => Text -> Int -> ClientContext -> DataSourceUpdates -> m ()
streamingThread streamURI initialRetryDelay clientContext dataSourceUpdates = do
    let manager = tlsManager $ httpConfiguration clientContext
    req <- prepareRequest (httpConfiguration clientContext) (T.unpack streamURI ++ "/all")
    handleUnauthorized dataSourceUpdates (processStream manager req)
  where
    processStream :: (MonadIO m, MonadLogger m, MonadMask m) => Manager -> Request -> m ()
    processStream manager req = do
        void $ iterateUntilM (cancel) (\state -> startNewConnection manager req dataSourceUpdates state) StreamingState {initialConnection = True, initialRetryDelay, activeSince = Nothing, attempt = 0, cancel = False}
