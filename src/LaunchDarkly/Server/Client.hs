-- | This module contains the core functionality of the SDK.
module LaunchDarkly.Server.Client
    ( Client
    , makeClient
    , clientVersion
    , boolVariation
    , boolVariationDetail
    , stringVariation
    , stringVariationDetail
    , intVariation
    , intVariationDetail
    , doubleVariation
    , doubleVariationDetail
    , jsonVariation
    , jsonVariationDetail
    , EvaluationDetail (..)
    , EvaluationReason (..)
    , EvalErrorKind (..)
    , allFlagsState
    , AllFlagsState
    , secureModeHash
    , close
    , flushEvents
    , identify
    , track
    , Status (..)
    , getStatus
    ) where

import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, void)
import Control.Monad.Fix (mfix)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logDebug, logWarn)
import Data.Aeson (ToJSON, Value (..), object, toJSON, (.=))
import Data.Generics.Product (getField)
import qualified Data.HashSet as HS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Scientific (fromFloatDigits, toRealFloat)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Network.HTTP.Client (newManager)
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Clock (TimeSpec (..))

import LaunchDarkly.AesonCompat (KeyMap, emptyObject, filterObject, insertKey, mapValues)
import LaunchDarkly.Server.Client.Internal (Client (..), clientVersion, getStatusI)
import LaunchDarkly.Server.Client.Status (Status (..))
import LaunchDarkly.Server.Config.ClientContext (ClientContext (..))
import LaunchDarkly.Server.Config.HttpConfiguration (HttpConfiguration (..))
import LaunchDarkly.Server.Config.Internal (ApplicationInfo, Config, getApplicationInfoHeader, shouldSendEvents)
import LaunchDarkly.Server.Context (getValue)
import LaunchDarkly.Server.Context.Internal (Context (Invalid), getCanonicalKey, getKey, getKeys, optionallyRedactAnonymous, redactContext)
import LaunchDarkly.Server.DataSource.Internal (DataSource (..), DataSourceFactory, DataSourceUpdates (..), defaultDataSourceUpdates, nullDataSourceFactory)
import LaunchDarkly.Server.Details (EvalErrorKind (..), EvaluationDetail (..), EvaluationReason (..))
import LaunchDarkly.Server.Evaluate (evaluateDetail, evaluateTyped)
import LaunchDarkly.Server.Events (CustomEvent (..), EventType (..), IdentifyEvent (..), makeBaseEvent, makeEventState, maybeIndexContext, noticeContext, queueEvent, unixMilliseconds)
import LaunchDarkly.Server.Features (isClientSideOnlyFlag, isInExperiment)
import LaunchDarkly.Server.Network.Eventing (eventThread)
import LaunchDarkly.Server.Network.Polling (pollingThread)
import LaunchDarkly.Server.Network.Streaming (streamingThread)
import LaunchDarkly.Server.Store.Internal (getAllFlagsC, makeStoreIO)

import Crypto.Hash.SHA256 (hash)
import Crypto.MAC.HMAC (hmac)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (HeaderName)

networkDataSourceFactory :: (ClientContext -> DataSourceUpdates -> LoggingT IO ()) -> DataSourceFactory
networkDataSourceFactory threadF clientContext dataSourceUpdates = do
    initialized <- liftIO $ newIORef False
    thread <- liftIO newEmptyMVar
    sync <- liftIO newEmptyMVar

    let dataSourceIsInitialized = readIORef initialized

        dataSourceStart = do
            putMVar thread =<< forkFinally (runLogger clientContext $ threadF clientContext dataSourceUpdates) (\_ -> putMVar sync ())
            writeIORef initialized True

        dataSourceStop = runLogger clientContext $ do
            $(logDebug) "Killing download thread"
            liftIO $ killThread =<< takeMVar thread
            $(logDebug) "Waiting on download thread to die"
            liftIO $ void $ takeMVar sync

    pure $ DataSource {..}

makeHttpConfiguration :: Config -> IO HttpConfiguration
makeHttpConfiguration config = do
    tlsManager <- newManager tlsManagerSettings
    let headers =
            [ ("Authorization", encodeUtf8 $ getField @"key" config)
            , ("User-Agent", "HaskellServerClient/" <> encodeUtf8 clientVersion)
            ]
        defaultRequestHeaders = addTagsHeader headers (getField @"applicationInfo" config)
        defaultRequestTimeout = Http.responseTimeoutMicro $ fromIntegral $ getField @"requestTimeoutSeconds" config * 1000000
    pure $ HttpConfiguration {..}
  where
    addTagsHeader :: [(HeaderName, ByteString)] -> Maybe ApplicationInfo -> [(HeaderName, ByteString)]
    addTagsHeader headers Nothing = headers
    addTagsHeader headers (Just info) = case getApplicationInfoHeader info of
        Nothing -> headers
        Just header -> ("X-LaunchDarkly-Tags", encodeUtf8 header) : headers

makeClientContext :: Config -> IO ClientContext
makeClientContext config = do
    let runLogger = getField @"logger" config
    httpConfiguration <- makeHttpConfiguration config
    pure $ ClientContext {..}

-- | Create a new instance of the LaunchDarkly client.
makeClient :: Config -> IO Client
makeClient config = mfix $ \client -> do
    status <- newIORef Uninitialized
    store <- makeStoreIO (getField @"storeBackend" config) (TimeSpec (fromIntegral $ getField @"storeTTLSeconds" config) 0)
    manager <- case getField @"manager" config of
        Just manager -> pure manager
        Nothing -> newManager tlsManagerSettings
    events <- makeEventState config

    clientContext <- makeClientContext config

    let dataSourceUpdates = defaultDataSourceUpdates status store
    dataSource <- getDataSourceFactory config clientContext dataSourceUpdates
    eventThreadPair <-
        if not (shouldSendEvents config)
            then pure Nothing
            else do
                sync <- newEmptyMVar
                thread <- forkFinally (runLogger clientContext $ eventThread manager client clientContext) (\_ -> putMVar sync ())
                pure $ pure (thread, sync)

    dataSourceStart dataSource

    pure $ Client {..}

getDataSourceFactory :: Config -> DataSourceFactory
getDataSourceFactory config =
    if getField @"offline" config || getField @"useLdd" config
        then nullDataSourceFactory
        else case getField @"dataSourceFactory" config of
            Just factory ->
                factory
            Nothing ->
                let dataSourceThread =
                        if getField @"streaming" config
                            then streamingThread (getField @"streamURI" config) (getField @"initialRetryDelay" config)
                            else pollingThread (getField @"baseURI" config) (getField @"pollIntervalSeconds" config)
                 in networkDataSourceFactory dataSourceThread

clientRunLogger :: Client -> (LoggingT IO () -> IO ())
clientRunLogger client = getField @"logger" $ getField @"config" client

-- | Return the initialization status of the Client
getStatus :: Client -> IO Status
getStatus client = getStatusI client

-- TODO(mmk) This method exists in multiple places. Should we move this into a
-- util file?
fromObject :: Value -> KeyMap Value
fromObject x = case x of (Object o) -> o; _ -> error "expected object"

-- |
-- AllFlagsState captures the state of all feature flag keys as evaluated for
-- a specific context. This includes their values, as well as other metadata.
data AllFlagsState = AllFlagsState
    { evaluations :: !(KeyMap Value)
    , state :: !(KeyMap FlagState)
    , valid :: !Bool
    }
    deriving (Show, Generic)

instance ToJSON AllFlagsState where
    toJSON state =
        Object $
            insertKey "$flagsState" (toJSON $ getField @"state" state) $
                insertKey
                    "$valid"
                    (toJSON $ getField @"valid" state)
                    (fromObject $ toJSON $ getField @"evaluations" state)

data FlagState = FlagState
    { version :: !(Maybe Natural)
    , variation :: !(Maybe Integer)
    , reason :: !(Maybe EvaluationReason)
    , trackEvents :: !Bool
    , trackReason :: !Bool
    , debugEventsUntilDate :: !(Maybe Natural)
    }
    deriving (Show, Generic)

instance ToJSON FlagState where
    toJSON state =
        object $
            filter
                ((/=) Null . snd)
                [ "version" .= getField @"version" state
                , "variation" .= getField @"variation" state
                , "trackEvents" .= if getField @"trackEvents" state then Just True else Nothing
                , "trackReason" .= if getField @"trackReason" state then Just True else Nothing
                , "reason" .= getField @"reason" state
                , "debugEventsUntilDate" .= getField @"debugEventsUntilDate" state
                ]

-- |
-- Returns an object that encapsulates the state of all feature flags for a
-- given context. This includes the flag values, and also metadata that can be
-- used on the front end.
--
-- The most common use case for this method is to bootstrap a set of
-- client-side feature flags from a back-end service.
--
-- The first parameter will limit to only flags that are marked for use with
-- the client-side SDK (by default, all flags are included).
--
-- The second parameter will include evaluation reasons in the state.
--
-- The third parameter will omit any metadata that is normally only used for
-- event generation, such as flag versions and evaluation reasons, unless the
-- flag has event tracking or debugging turned on
--
-- For more information, see the Reference Guide:
-- https://docs.launchdarkly.com/sdk/features/all-flags#haskell
allFlagsState :: Client -> Context -> Bool -> Bool -> Bool -> IO (AllFlagsState)
allFlagsState client context client_side_only with_reasons details_only_for_tracked_flags = do
    status <- getAllFlagsC $ getField @"store" client
    case status of
        Left _ -> pure AllFlagsState {evaluations = emptyObject, state = emptyObject, valid = False}
        Right flags -> do
            filtered <- pure $ (filterObject (\flag -> (not client_side_only) || isClientSideOnlyFlag flag) flags)
            details <- mapM (\flag -> (\detail -> (flag, fst detail)) <$> (evaluateDetail flag context HS.empty $ getField @"store" client)) filtered
            evaluations <- pure $ mapValues (getField @"value" . snd) details
            now <- unixMilliseconds
            state <-
                pure $
                    mapValues
                        ( \(flag, detail) -> do
                            let reason' = getField @"reason" detail
                                inExperiment = isInExperiment flag reason'
                                isDebugging = now < fromMaybe 0 (getField @"debugEventsUntilDate" flag)
                                trackReason' = inExperiment
                                trackEvents' = getField @"trackEvents" flag
                                omitDetails = details_only_for_tracked_flags && (not (trackEvents' || trackReason' || isDebugging))
                            FlagState
                                { version = if omitDetails then Nothing else Just $ getField @"version" flag
                                , variation = getField @"variationIndex" detail
                                , reason = if omitDetails || ((not with_reasons) && (not trackReason')) then Nothing else Just reason'
                                , trackEvents = trackEvents' || inExperiment
                                , trackReason = trackReason'
                                , debugEventsUntilDate = getField @"debugEventsUntilDate" flag
                                }
                        )
                        details
            pure $ AllFlagsState {evaluations = evaluations, state = state, valid = True}

-- | Identify reports details about a context.
identify :: Client -> Context -> IO ()
identify client (Invalid err) = clientRunLogger client $ $(logWarn) $ "identify called with an invalid context: " <> err
identify client context = case (getValue "key" context) of
    (String "") -> clientRunLogger client $ $(logWarn) "identify called with empty key"
    _anyValidKey -> do
        let identifyContext = optionallyRedactAnonymous (getField @"config" client) context
        case identifyContext of
            (Invalid _) -> pure ()
            _anyValidContext -> do
                let redacted = redactContext (getField @"config" client) identifyContext
                x <- makeBaseEvent $ IdentifyEvent {key = getKey context, context = redacted}
                _ <- noticeContext (getField @"events" client) context
                queueEvent (getField @"config" client) (getField @"events" client) (EventTypeIdentify x)

-- |
-- Track reports that a context has performed an event. Custom data can be
-- attached to the event, and / or a numeric value.
--
-- The numeric value is used by the LaunchDarkly experimentation feature in
-- numeric custom metrics, and will also be returned as part of the custom
-- event for Data Export.
track :: Client -> Context -> Text -> Maybe Value -> Maybe Double -> IO ()
track client (Invalid err) _ _ _ = clientRunLogger client $ $(logWarn) $ "track called with invalid context: " <> err
track client context key value metric = do
    x <-
        makeBaseEvent $
            CustomEvent
                { key = key
                , contextKeys = getKeys context
                , metricValue = metric
                , value = value
                }
    let config = (getField @"config" client)
        events = (getField @"events" client)
    queueEvent config events (EventTypeCustom x)
    unixMilliseconds >>= \now -> maybeIndexContext now config context events

-- |
-- Generates the secure mode hash value for a context.
--
-- For more information, see the Reference Guide:
-- <https://docs.launchdarkly.com/sdk/features/secure-mode#haskell>.
secureModeHash :: Client -> Context -> Text
secureModeHash client context =
    let config = getField @"config" client
        sdkKey = getField @"key" config
     in decodeUtf8 $ convertToBase Base16 $ hmac hash 64 (encodeUtf8 sdkKey) (encodeUtf8 $ getCanonicalKey context)

-- |
-- Flush tells the client that all pending analytics events (if any) should
-- be delivered as soon as possible. Flushing is asynchronous, so this method
-- will return before it is complete.
flushEvents :: Client -> IO ()
flushEvents client = putMVar (getField @"flush" $ getField @"events" client) ()

-- |
-- Close shuts down the LaunchDarkly client. After calling this, the
-- LaunchDarkly client should no longer be used. The method will block until
-- all pending analytics events have been sent.
close :: Client -> IO ()
close client = clientRunLogger client $ do
    $(logDebug) "Setting client status to ShuttingDown"
    liftIO $ writeIORef (getField @"status" client) ShuttingDown
    liftIO $ dataSourceStop $ getField @"dataSource" client
    forM_ (getField @"eventThreadPair" client) $ \(_, sync) -> do
        $(logDebug) "Triggering event flush"
        liftIO $ flushEvents client
        $(logDebug) "Waiting on event thread to die"
        liftIO $ void $ takeMVar sync
    $(logDebug) "Client background resources destroyed"

type ValueConverter a = (a -> Value, Value -> Maybe a)

reorderStuff :: ValueConverter a -> Bool -> Client -> Text -> Context -> a -> IO (EvaluationDetail a)
reorderStuff converter includeReason client key context fallback = evaluateTyped client key context fallback (fst converter) includeReason (snd converter)

dropReason :: (Text -> Context -> a -> IO (EvaluationDetail a)) -> Text -> Context -> a -> IO a
dropReason = (((fmap (getField @"value") .) .) .)

boolConverter :: ValueConverter Bool
boolConverter = (,) Bool $ \case Bool x -> pure x; _ -> Nothing

stringConverter :: ValueConverter Text
stringConverter = (,) String $ \case String x -> pure x; _ -> Nothing

intConverter :: ValueConverter Int
intConverter = (,) (Number . fromIntegral) $ \case Number x -> pure $ truncate x; _ -> Nothing

doubleConverter :: ValueConverter Double
doubleConverter = (,) (Number . fromFloatDigits) $ \case Number x -> pure $ toRealFloat x; _ -> Nothing

jsonConverter :: ValueConverter Value
jsonConverter = (,) id pure

-- | Evaluate a Boolean typed flag.
boolVariation :: Client -> Text -> Context -> Bool -> IO Bool
boolVariation = dropReason . reorderStuff boolConverter False

-- | Evaluate a Boolean typed flag, and return an explanation.
boolVariationDetail :: Client -> Text -> Context -> Bool -> IO (EvaluationDetail Bool)
boolVariationDetail = reorderStuff boolConverter True

-- | Evaluate a String typed flag.
stringVariation :: Client -> Text -> Context -> Text -> IO Text
stringVariation = dropReason . reorderStuff stringConverter False

-- | Evaluate a String typed flag, and return an explanation.
stringVariationDetail :: Client -> Text -> Context -> Text -> IO (EvaluationDetail Text)
stringVariationDetail = reorderStuff stringConverter True

-- | Evaluate a Number typed flag, and truncate the result.
intVariation :: Client -> Text -> Context -> Int -> IO Int
intVariation = dropReason . reorderStuff intConverter False

-- |
-- Evaluate a Number typed flag, truncate the result, and return an
-- explanation.
intVariationDetail :: Client -> Text -> Context -> Int -> IO (EvaluationDetail Int)
intVariationDetail = reorderStuff intConverter True

-- | Evaluate a Number typed flag.
doubleVariation :: Client -> Text -> Context -> Double -> IO Double
doubleVariation = dropReason . reorderStuff doubleConverter False

-- | Evaluate a Number typed flag, and return an explanation.
doubleVariationDetail :: Client -> Text -> Context -> Double -> IO (EvaluationDetail Double)
doubleVariationDetail = reorderStuff doubleConverter True

-- | Evaluate a JSON typed flag.
jsonVariation :: Client -> Text -> Context -> Value -> IO Value
jsonVariation = dropReason . reorderStuff jsonConverter False

-- | Evaluate a JSON typed flag, and return an explanation.
jsonVariationDetail :: Client -> Text -> Context -> Value -> IO (EvaluationDetail Value)
jsonVariationDetail = reorderStuff jsonConverter True
