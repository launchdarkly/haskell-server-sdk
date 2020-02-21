module LaunchDarkly.Server.Network.Streaming (streamingThread) where

import           Data.Text                           (Text)
import qualified Data.Text as                        T
import           Data.Attoparsec.Text as             P hiding (Result)
import           Control.Monad                       (void, mzero, forever)
import           Data.ByteString                     (ByteString)
import           Control.Applicative                 (many)
import           Data.Text.Encoding                  (decodeUtf8, encodeUtf8)
import           Data.HashMap.Strict                 (HashMap)
import           Network.HTTP.Client                 (Manager, Response(..), Request, parseRequest, brRead, throwErrorStatusCodes)
import           Control.Monad.Logger                (MonadLogger, logInfo, logWarn, logError)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Data.Generics.Product               (getField)
import           Data.Aeson                          (FromJSON, parseJSON, withObject, eitherDecode, (.:), fromJSON, Result(..))
import qualified Data.ByteString.Lazy as             L
import           GHC.Natural                         (Natural)
import           GHC.Generics                        (Generic)
import           Control.Monad.Catch                 (MonadMask)
import           Control.Retry                       (RetryPolicyM, RetryStatus, fullJitterBackoff, capDelay, retrying)
import           Network.HTTP.Types.Status           (ok200)

import           LaunchDarkly.Server.Client.Internal (ClientI, Status(Initialized), setStatus)
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

processPut :: (MonadIO m, MonadLogger m) => ClientI -> StoreHandle IO -> L.ByteString -> m ()
processPut client store value = case eitherDecode value of
    Right (PathData _ (PutBody flags segments)) -> do
        $(logInfo) "initializing store with put"
        status <- liftIO $ initializeStore store flags segments
        either (\err -> $(logError) $ T.append "store failed put: " err)
               (\_   -> liftIO $ setStatus client Initialized) status
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

processEvent :: (MonadIO m, MonadLogger m) => ClientI -> StoreHandle IO -> Text -> L.ByteString -> m ()
processEvent client store name value = case name of
    "put"    -> processPut client store value
    "patch"  -> processPatch store value
    "delete" -> processDelete store value
    _        -> $(logWarn) "unknown event type"

readStream :: (MonadIO m, MonadLogger m) => ClientI -> IO ByteString -> StoreHandle IO -> m ()
readStream client body store = loop "" where
    loop initial = liftIO (parseWith (decodeUtf8 <$> brRead body) parseEvent initial) >>= \case
        Done remaining event -> do
            processEvent client store (name event) (L.fromStrict $ encodeUtf8 $ buffer event)
            loop remaining
        Fail _ context err ->
            $(logError) $ T.intercalate " " ["failed parsing SSE frame", T.pack $ show context, T.pack err]
        Partial _ -> $(logError) "failed parsing SSE frame unexpected partial"

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
    req <- prepareRequest config <$> liftIO (parseRequest $ T.unpack (getField @"streamURI" config) ++ "/all")
    tryAuthorized client $ forever $ retrying retryPolicy (\_ status -> pure status) $ handleStream client manager req store
