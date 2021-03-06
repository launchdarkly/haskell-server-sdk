module LaunchDarkly.Server.Network.Streaming (streamingThread) where

import           Data.Text                           (Text)
import qualified Data.Text as                        T
import           Data.Attoparsec.Text as             P hiding (Result, try)
import           Data.Function                       (fix)
import           Control.Monad                       (void, mzero)
import           Control.Monad.Catch                 (Exception, MonadCatch, try)
import           Control.Exception                   (throwIO)
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString as                  B
import           Control.Applicative                 (many)
import           Data.Text.Encoding                  (decodeUtf8, encodeUtf8)
import           Data.HashMap.Strict                 (HashMap)
import           Network.HTTP.Client                 (Manager, Response(..), Request, HttpException(..), HttpExceptionContent(..), parseRequest, brRead, throwErrorStatusCodes)
import           Control.Monad.Logger                (MonadLogger, logInfo, logWarn, logError, logDebug)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Data.Generics.Product               (getField)
import           Data.Aeson                          (FromJSON, parseJSON, withObject, eitherDecode, (.:), fromJSON, Result(..))
import qualified Data.ByteString.Lazy as             L
import           GHC.Natural                         (Natural)
import           GHC.Generics                        (Generic)
import           Control.Monad.Catch                 (MonadMask)
import           Control.Retry                       (RetryPolicyM, RetryStatus, fullJitterBackoff, capDelay, retrying)
import           Network.HTTP.Types.Status           (ok200, Status(statusCode))
import           System.Timeout                      (timeout)
import           System.Clock                        (getTime, Clock(Monotonic), TimeSpec(TimeSpec))

import           LaunchDarkly.Server.Client.Internal (ClientI)
import           LaunchDarkly.Server.Store.Internal  (StoreHandle, StoreResult, initializeStore, insertFlag, deleteFlag, deleteSegment, insertSegment)
import           LaunchDarkly.Server.Features        (Flag, Segment)
import           LaunchDarkly.Server.Network.Common  (tryAuthorized, checkAuthorization, prepareRequest, withResponseGeneric, tryHTTP)

data PutBody = PutBody
    { flags    :: !(HashMap Text Flag)
    , segments :: !(HashMap Text Segment)
    } deriving (Generic, Show, FromJSON)

data PathData d = PathData
    { path     :: !Text
    , pathData :: !d
    } deriving (Generic, Show)

data PathVersion = PathVersion
    { path    :: !Text
    , version :: !Natural
    } deriving (Generic, Show, FromJSON)

instance FromJSON a => FromJSON (PathData a) where
    parseJSON = withObject "Put" $ \o -> do
        pathData <- o .: "data"
        path     <- o .: "path"
        pure $ PathData { path = path, pathData = pathData }

data SSE = SSE
    { name        :: !Text
    , buffer      :: !Text
    , lastEventId :: !(Maybe Text)
    , retry       :: !(Maybe Text)
    } deriving (Generic, Show, Eq)

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
    "event"   -> event { name = fieldValue }
    "id"      -> event { lastEventId = Just fieldValue }
    "retry"   -> event { retry = Just fieldValue }
    "data"    -> event { buffer = T.concat [ buffer event, if T.null (buffer event) then "" else "\n", fieldValue] }
    _         -> event

parseEvent :: Parser SSE
parseEvent = do
    fields <- many (many comment >> parseField >>= pure)
    endOfLineSSE
    let event = foldr processField (SSE "" "" mzero mzero) fields
    if T.null (name event) || T.null (buffer event) then parseEvent else pure event

processPut :: (MonadIO m, MonadLogger m) => StoreHandle IO -> L.ByteString -> m ()
processPut store value = case eitherDecode value of
    Right (PathData _ (PutBody flags segments)) -> do
        $(logInfo) "initializing store with put"
        liftIO (initializeStore store flags segments) >>= \case
            Left err -> $(logError) $ T.append "store failed put: " err
            _        -> pure ()
    Left err -> $(logError) $ T.append "failed to parse put body" (T.pack err)

processPatch :: forall m. (MonadIO m, MonadLogger m) => StoreHandle IO -> L.ByteString -> m ()
processPatch store value = case eitherDecode value of
    Right (PathData path body)
        | T.isPrefixOf "/flags/" path    -> insPatch "flag" path insertFlag (fromJSON body)
        | T.isPrefixOf "/segments/" path -> insPatch "segment" path insertSegment (fromJSON body)
        | otherwise                      -> $(logError) "unknown patch path"
    Left err                             -> $(logError) $ T.append "failed to parse patch generic" (T.pack err)
    where
      insPatch :: Text -> Text -> (StoreHandle IO -> a -> StoreResult ()) -> Result a -> m ()
      insPatch name _ _ (Error err) = $(logError) $ T.concat ["failed to parse patch ", name, ": ", T.pack err]
      insPatch name path insert (Success item) = do
        $(logInfo) $ T.concat ["patching ", name, " with path: ", path]
        status <- liftIO $ insert store item
        either (\err -> $(logError) $ T.concat ["store failed ", name, " patch: ", err]) pure status

processDelete :: forall m. (MonadIO m, MonadLogger m) => StoreHandle IO -> L.ByteString -> m ()
processDelete store value = case eitherDecode value :: Either String PathVersion of
    Right (PathVersion path version)
        | T.isPrefixOf "/flags/" path    -> logDelete "flag" path (deleteFlag store (T.drop 7 path) version)
        | T.isPrefixOf "/segments/" path -> logDelete "segment" path (deleteSegment store (T.drop 10 path) version)
        | otherwise                      -> $(logError) "unknown delete path"
    Left err                             -> $(logError) $ T.append "failed to parse delete" (T.pack err)
    where logDelete :: Text -> Text -> StoreResult () -> m ()
          logDelete name path action = do
            $(logInfo) $ T.concat ["deleting ", name, " with path: ", path]
            status <- liftIO action
            either (\err -> $(logError) $ T.concat ["store failed ", name, " delete: ", err]) pure status

processEvent :: (MonadIO m, MonadLogger m) => StoreHandle IO -> Text -> L.ByteString -> m ()
processEvent store name value = case name of
    "put"    -> processPut store value
    "patch"  -> processPatch store value
    "delete" -> processDelete store value
    _        -> $(logWarn) "unknown event type"

data ReadE = ReadETimeout | ReadEClosed deriving (Show, Exception)

tryReadE :: MonadCatch m => m a -> m (Either ReadE a)
tryReadE = try

-- heartbeat expected every 120 seconds
readWithException :: IO ByteString -> IO Text
readWithException body = timeout (1000000 * 300) (brRead body) >>= \case
    Nothing    -> throwIO ReadETimeout
    Just bytes -> if bytes == B.empty then throwIO ReadEClosed else pure (decodeUtf8 bytes)

readStream :: (MonadIO m, MonadLogger m, MonadMask m) => IO ByteString -> StoreHandle IO -> m ()
readStream body store = loop "" where
    loop initial = tryReadE (parseWith (liftIO $ readWithException body) parseEvent initial) >>= \case
        (Left ReadETimeout) -> $(logError) "streaming connection unexpectedly closed"
        (Left ReadEClosed)  -> $(logError) "timeout waiting for SSE event"
        (Right parsed)      -> case parsed of
            Done remaining event -> do
                processEvent store (name event) (L.fromStrict $ encodeUtf8 $ buffer event)
                loop remaining
            Fail _ context err ->
                $(logError) $ T.intercalate " " ["failed parsing SSE frame", T.pack $ show context, T.pack err]
            Partial _ -> $(logError) "failed parsing SSE frame unexpected partial"

-- https://aws.amazon.com/blogs/architecture/exponential-backoff-and-jitter/
retryPolicy :: MonadIO m => RetryPolicyM m
retryPolicy = capDelay (30 * 1000000) $ fullJitterBackoff (1 * 1000000)

data Failure = FailurePermanent | FailureTemporary | FailureReset deriving (Eq)

handleStream :: (MonadIO m, MonadLogger m, MonadMask m) => Manager -> Request -> StoreHandle IO -> RetryStatus -> m Failure
handleStream manager request store _ = do
    $(logDebug) "starting new streaming connection"
    startTime <- liftIO $ getTime Monotonic
    status <- tryHTTP $ withResponseGeneric request manager $ \response -> checkAuthorization response >>
        if responseStatus response /= ok200 then throwErrorStatusCodes request response else
            readStream (responseBody response) store
    case status of
        (Right ()) -> liftIO (getTime Monotonic) >>= \now -> if now >= startTime + (TimeSpec 60 0)
            then do
                $(logError) "streaming connection closed after 60 seconds, retrying instantly"
                pure FailureReset
            else do
                $(logError) "streaming connection closed before 60 seconds, retrying after backoff"
                pure FailureTemporary
        (Left err) -> do
            $(logError) (T.intercalate " " ["HTTP Exception", T.pack $ show err])
            pure $ case err of
                (InvalidUrlException _ _)                                 -> FailurePermanent
                (HttpExceptionRequest _ (StatusCodeException response _)) ->
                    let code = statusCode (responseStatus response) in if code >= 400 && code < 500
                        then if elem code [400, 408, 429] then FailureTemporary else FailurePermanent
                        else FailureTemporary
                _                                                         -> FailureTemporary

streamingThread :: (MonadIO m, MonadLogger m, MonadMask m) => Manager -> ClientI -> m ()
streamingThread manager client = do
    let config = getField @"config" client; store = getField @"store" client;
    req <- prepareRequest config <$> liftIO (parseRequest $ T.unpack (getField @"streamURI" config) ++ "/all")
    tryAuthorized client $ fix $ \loop ->
        retrying retryPolicy (\_ status -> pure $ status == FailureTemporary) (handleStream manager req store) >>=
            \case FailureReset -> loop; _ -> pure ()
