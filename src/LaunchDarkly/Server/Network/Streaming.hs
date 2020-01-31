module LaunchDarkly.Server.Network.Streaming (streamingThread) where

import           Data.Text                           (Text)
import qualified Data.Text as                        T
import           Data.Attoparsec.Text as             P
import           Control.Monad                       (void, mzero)
import           Data.ByteString                     (ByteString)
import           Control.Applicative                 (many)
import           Data.Text.Encoding                  (decodeUtf8, encodeUtf8)
import           Data.HashMap.Strict                 (HashMap)
import           Network.HTTP.Client                 (Manager, Response(..), Request, parseRequest, brRead, throwErrorStatusCodes)
import           Control.Monad.Logger                (MonadLogger, logInfo, logWarn, logError)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Data.Generics.Product               (getField)
import           Data.Aeson                          (FromJSON, parseJSON, withObject, eitherDecode, (.:), Object, encode)
import qualified Data.ByteString.Lazy as             L
import           GHC.Natural                         (Natural)
import           GHC.Generics                        (Generic)
import           Control.Monad.Catch                 (MonadMask)
import           Control.Monad                       (forever)
import           Control.Retry                       (RetryPolicyM, RetryStatus, fullJitterBackoff, capDelay, retrying)
import           Network.HTTP.Types.Status           (ok200)

import           LaunchDarkly.Server.Client.Internal (ClientI, Status(Initialized), setStatus)
import           LaunchDarkly.Server.Store.Internal  (StoreHandle, initializeStore, insertFlag, deleteFlag, deleteSegment, insertSegment)
import           LaunchDarkly.Server.Features        (Flag, Segment)
import           LaunchDarkly.Server.Network.Common  (tryAuthorized, checkAuthorization, prepareRequest, withResponseGeneric, tryHTTP)

data PutBody = PutBody
    { flags    :: HashMap Text Flag
    , segments :: HashMap Text Segment
    } deriving (Generic, Show, FromJSON)

data PathData d = PathData
    { path     :: Text
    , pathData :: d
    } deriving (Generic, Show)

data PathVersion = PathVersion
    { path    :: Text
    , version :: Natural
    } deriving (Generic, Show, FromJSON)

instance FromJSON a => FromJSON (PathData a) where
    parseJSON = withObject "Put" $ \o -> do
        pathData <- o .: "data"
        path     <- o .: "path"
        pure $ PathData { path = path, pathData = pathData }

data SSE = SSE
    { name        :: Text
    , buffer      :: Text
    , lastEventId :: Maybe Text
    , retry       :: Maybe Text
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
    if (T.null $ name event) || (T.null $ buffer event) then parseEvent else pure event

processPut :: (MonadIO m, MonadLogger m) => ClientI -> StoreHandle IO -> L.ByteString -> m ()
processPut client store value = case eitherDecode value :: Either String (PathData PutBody) of
    Right (PathData _ (PutBody flags segments)) -> do
        $(logInfo) "initializing store with put"
        status <- liftIO $ initializeStore store flags segments
        case status of
            Right () -> liftIO $ setStatus client Initialized
            Left err -> $(logError) $ T.append "store failed put: " err
    Left err -> do
        $(logError) $ T.append "failed to parse put body" (T.pack err)

processPatch :: (MonadIO m, MonadLogger m) => StoreHandle IO -> L.ByteString -> m ()
processPatch store value = case eitherDecode value :: Either String (PathData Object) of
    Right (PathData path body)
        | T.isPrefixOf "/flags/" path -> do
            case eitherDecode (encode body) :: Either String (Flag) of
                (Right flag) -> do
                    $(logInfo) $ T.append "patching flag with path " path
                    status <- liftIO $ insertFlag store flag
                    case status of
                        Right () -> pure ()
                        Left err -> do
                            $(logError) $ T.append "store failed flag patch: " err
                (Left err)   -> do
                    $(logError) $ T.append "failed to parse patch flag" (T.pack err)
        | T.isPrefixOf "/segments/" path -> do
            case eitherDecode (encode body) :: Either String (Segment) of
                (Right segment) -> do
                    $(logInfo) $ T.append "patching segment with path " path
                    status <- liftIO $ insertSegment store segment
                    case status of
                        Right () -> pure ()
                        Left err -> do
                            $(logError) $ T.append "store failed segment patch: " err
                (Left err)   -> do
                    $(logError) $ T.append "failed to parse patch segment" (T.pack err)
        | otherwise -> $(logError) "unknown patch path"
    Left err -> do
        $(logError) $ T.append "failed to parse patch generic" (T.pack err)

processDelete :: (MonadIO m, MonadLogger m) => StoreHandle IO -> L.ByteString -> m ()
processDelete store value = case eitherDecode value :: Either String (PathVersion) of
    Right (PathVersion path version)
        | T.isPrefixOf "/flags/" path -> do
            $(logInfo) $ T.append "deleting flag with path " path
            status <- liftIO $ deleteFlag store (T.drop 7 path) version
            case status of
                Right () -> pure ()
                Left err -> do
                    $(logError) $ T.append "store failed flag delete: " err
        | T.isPrefixOf "/segments/" path -> do
            $(logInfo) $ T.append "deleting segment with path " path
            status <- liftIO $ deleteSegment store (T.drop 10 path) version
            case status of
                Right () -> pure ()
                Left err -> do
                    $(logError) $ T.append "store failed segment delete: " err
        | otherwise -> $(logError) "unknown delete path"
    Left err -> do
        $(logError) $ T.append "failed to parse delete" (T.pack err)

processEvent :: (MonadIO m, MonadLogger m) => ClientI -> StoreHandle IO -> Text -> L.ByteString -> m ()
processEvent client store name value = case name of
    "put"    -> processPut client store value
    "patch"  -> processPatch store value
    "delete" -> processDelete store value
    _        -> $(logWarn) "unknown event type"

readStream :: (MonadIO m, MonadLogger m) => ClientI -> (IO ByteString) -> StoreHandle IO -> m ()
readStream client body store = loop "" where
    loop initial = (liftIO $ parseWith (fmap decodeUtf8 $ brRead body) parseEvent initial) >>= \case
        (Done remaining event) -> do
            processEvent client store (name event) (L.fromStrict $ encodeUtf8 $ buffer event)
            loop remaining
        (Fail _ context err) -> do
            $(logError) $ T.intercalate " " ["failed parsing SSE frame", T.pack $ show context, T.pack err]
        (Partial _) -> $(logError) "failed parsing SSE frame unexpected partial"

-- https://aws.amazon.com/blogs/architecture/exponential-backoff-and-jitter/
retryPolicy :: MonadIO m => RetryPolicyM m
retryPolicy = capDelay (3600 * 1000000) $ fullJitterBackoff (1 * 1000000)

handleStream :: (MonadIO m, MonadLogger m, MonadMask m) => ClientI -> Manager -> Request -> StoreHandle IO -> RetryStatus -> m Bool
handleStream client manager request store _ = do
    status <- tryHTTP $ withResponseGeneric request manager $ \response -> checkAuthorization response >>
        if responseStatus response /= ok200 then throwErrorStatusCodes request response else
            readStream client (responseBody response) store
    case status of
        (Left err) -> $(logError) (T.intercalate " " ["HTTP Exception", T.pack $ show err]) >> pure True
        (Right _)  -> pure False

streamingThread :: (MonadIO m, MonadLogger m, MonadMask m) => Manager -> ClientI -> m ()
streamingThread manager client = do
    let config = getField @"config" client; store = getField @"store" client;
    req <- (liftIO $ parseRequest $ (T.unpack $ getField @"streamURI" config) ++ "/all") >>= pure . prepareRequest config
    tryAuthorized client $ forever $ retrying retryPolicy (\_ status -> pure status) $ handleStream client manager req store
