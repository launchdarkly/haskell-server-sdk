module LaunchDarkly.Server.Network.Streaming (streamingThread) where

import Control.Applicative (many)
import Control.Exception (throwIO)
import Control.Monad (mzero, void)
import Control.Monad.Catch (Exception, MonadCatch, MonadMask, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebug, logError, logInfo, logWarn)
import Control.Retry (RetryPolicyM, RetryStatus, capDelay, fullJitterBackoff, retrying)
import Data.Aeson (FromJSON, Result (..), eitherDecode, fromJSON, parseJSON, withObject, (.!=), (.:), (.:?))
import Data.Attoparsec.Text as P hiding (Result, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Function (fix)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), Manager, Request, Response (..), brRead, throwErrorStatusCodes)
import Network.HTTP.Types.Status (Status (statusCode), ok200)
import System.Clock (Clock (Monotonic), TimeSpec (TimeSpec), getTime)
import System.Timeout (timeout)

import LaunchDarkly.AesonCompat (KeyMap)
import LaunchDarkly.Server.Config.ClientContext (ClientContext (..))
import LaunchDarkly.Server.Config.HttpConfiguration (HttpConfiguration (..), prepareRequest)
import LaunchDarkly.Server.DataSource.Internal (DataSourceUpdates (..))
import LaunchDarkly.Server.Features (Flag, Segment)
import LaunchDarkly.Server.Network.Common (checkAuthorization, handleUnauthorized, tryHTTP, withResponseGeneric)
import LaunchDarkly.Server.Store.Internal (StoreResult)

data PutBody = PutBody
    { flags :: !(KeyMap Flag)
    , segments :: !(KeyMap Segment)
    }
    deriving (Generic, Show, FromJSON)

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

processPut :: (MonadIO m, MonadLogger m) => DataSourceUpdates -> L.ByteString -> m ()
processPut dataSourceUpdates value = case eitherDecode value of
    Right (PathData _ (PutBody flags segments)) -> do
        $(logInfo) "initializing dataSourceUpdates with put"
        liftIO (dataSourceUpdatesInit dataSourceUpdates flags segments) >>= \case
            Left err -> $(logError) $ T.append "dataSourceUpdates failed put: " err
            _ -> pure ()
    Left err -> $(logError) $ T.append "failed to parse put body" (T.pack err)

processPatch :: forall m. (MonadIO m, MonadLogger m) => DataSourceUpdates -> L.ByteString -> m ()
processPatch dataSourceUpdates value = case eitherDecode value of
    Right (PathData path body)
        | T.isPrefixOf "/flags/" path -> insPatch "flag" path dataSourceUpdatesInsertFlag (fromJSON body)
        | T.isPrefixOf "/segments/" path -> insPatch "segment" path dataSourceUpdatesInsertSegment (fromJSON body)
        | otherwise -> $(logError) "unknown patch path"
    Left err -> $(logError) $ T.append "failed to parse patch generic" (T.pack err)
  where
    insPatch :: Text -> Text -> (DataSourceUpdates -> a -> IO (Either Text ())) -> Result a -> m ()
    insPatch name _ _ (Error err) = $(logError) $ T.concat ["failed to parse patch ", name, ": ", T.pack err]
    insPatch name path insert (Success item) = do
        $(logInfo) $ T.concat ["patching ", name, " with path: ", path]
        status <- liftIO $ insert dataSourceUpdates item
        either (\err -> $(logError) $ T.concat ["dataSourceUpdates failed ", name, " patch: ", err]) pure status

processDelete :: forall m. (MonadIO m, MonadLogger m) => DataSourceUpdates -> L.ByteString -> m ()
processDelete dataSourceUpdates value = case eitherDecode value :: Either String PathVersion of
    Right (PathVersion path version)
        | T.isPrefixOf "/flags/" path -> logDelete "flag" path (dataSourceUpdatesDeleteFlag dataSourceUpdates (T.drop 7 path) version)
        | T.isPrefixOf "/segments/" path -> logDelete "segment" path (dataSourceUpdatesDeleteSegment dataSourceUpdates (T.drop 10 path) version)
        | otherwise -> $(logError) "unknown delete path"
    Left err -> $(logError) $ T.append "failed to parse delete" (T.pack err)
  where
    logDelete :: Text -> Text -> StoreResult () -> m ()
    logDelete name path action = do
        $(logInfo) $ T.concat ["deleting ", name, " with path: ", path]
        status <- liftIO action
        either (\err -> $(logError) $ T.concat ["dataSourceUpdates failed ", name, " delete: ", err]) pure status

processEvent :: (MonadIO m, MonadLogger m) => DataSourceUpdates -> Text -> L.ByteString -> m ()
processEvent dataSourceUpdates name value = case name of
    "put" -> processPut dataSourceUpdates value
    "patch" -> processPatch dataSourceUpdates value
    "delete" -> processDelete dataSourceUpdates value
    _ -> $(logWarn) "unknown event type"

data ReadE = ReadETimeout | ReadEClosed deriving (Show, Exception)

tryReadE :: MonadCatch m => m a -> m (Either ReadE a)
tryReadE = try

-- heartbeat expected every 120 seconds
readWithException :: IO ByteString -> IO Text
readWithException body =
    timeout (1000000 * 300) (brRead body) >>= \case
        Nothing -> throwIO ReadETimeout
        Just bytes -> if bytes == B.empty then throwIO ReadEClosed else pure (decodeUtf8 bytes)

readStream :: (MonadIO m, MonadLogger m, MonadMask m) => IO ByteString -> DataSourceUpdates -> m ()
readStream body dataSourceUpdates = loop ""
  where
    loop initial =
        tryReadE (parseWith (liftIO $ readWithException body) parseEvent initial) >>= \case
            (Left ReadETimeout) -> $(logError) "streaming connection unexpectedly closed"
            (Left ReadEClosed) -> $(logError) "timeout waiting for SSE event"
            (Right parsed) -> case parsed of
                Done remaining event -> do
                    processEvent dataSourceUpdates (name event) (L.fromStrict $ encodeUtf8 $ buffer event)
                    loop remaining
                Fail _ context err ->
                    $(logError) $ T.intercalate " " ["failed parsing SSE frame", T.pack $ show context, T.pack err]
                Partial _ -> $(logError) "failed parsing SSE frame unexpected partial"

-- https://aws.amazon.com/blogs/architecture/exponential-backoff-and-jitter/
retryPolicy :: MonadIO m => RetryPolicyM m
retryPolicy = capDelay (30 * 1000000) $ fullJitterBackoff (1 * 1000000)

data Failure = FailurePermanent | FailureTemporary | FailureReset deriving (Eq)

handleStream :: (MonadIO m, MonadLogger m, MonadMask m) => Manager -> Request -> DataSourceUpdates -> RetryStatus -> m Failure
handleStream manager request dataSourceUpdates _ = do
    $(logDebug) "starting new streaming connection"
    startTime <- liftIO $ getTime Monotonic
    status <- tryHTTP $ withResponseGeneric request manager $ \response ->
        checkAuthorization response
            >> if responseStatus response /= ok200
                then throwErrorStatusCodes request response
                else readStream (responseBody response) dataSourceUpdates
    case status of
        (Right ()) ->
            liftIO (getTime Monotonic) >>= \now ->
                if now >= startTime + (TimeSpec 60 0)
                    then do
                        $(logError) "streaming connection closed after 60 seconds, retrying instantly"
                        pure FailureReset
                    else do
                        $(logError) "streaming connection closed before 60 seconds, retrying after backoff"
                        pure FailureTemporary
        (Left err) -> do
            $(logError) (T.intercalate " " ["HTTP Exception", T.pack $ show err])
            pure $ case err of
                (InvalidUrlException _ _) -> FailurePermanent
                (HttpExceptionRequest _ (StatusCodeException response _)) ->
                    let code = statusCode (responseStatus response)
                     in if code >= 400 && code < 500
                            then if elem code [400, 408, 429] then FailureTemporary else FailurePermanent
                            else FailureTemporary
                _ -> FailureTemporary

streamingThread :: (MonadIO m, MonadLogger m, MonadMask m) => Text -> ClientContext -> DataSourceUpdates -> m ()
streamingThread streamURI clientContext dataSourceUpdates = do
    let manager = tlsManager $ httpConfiguration clientContext
    req <- prepareRequest (httpConfiguration clientContext) (T.unpack streamURI ++ "/all")
    handleUnauthorized dataSourceUpdates $ fix $ \loop ->
        retrying retryPolicy (\_ status -> pure $ status == FailureTemporary) (handleStream manager req dataSourceUpdates)
            >>= \case FailureReset -> loop; _ -> pure ()
