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
    , EvaluationDetail(..)
    , EvaluationReason(..)
    , EvalErrorKind(..)
    , allFlagsState
    , AllFlagsState
    , close
    , flushEvents
    , identify
    , track
    , Status(..)
    , getStatus
    ) where

import           Control.Concurrent                    (forkFinally, killThread)
import           Control.Concurrent.MVar               (putMVar, takeMVar, newEmptyMVar)
import           Control.Monad                         (void, forM_)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Logger                  (LoggingT, logDebug, logWarn)
import           Control.Monad.Fix                     (mfix)
import           Data.IORef                            (newIORef, writeIORef, readIORef)
import           Data.Maybe                            (fromMaybe)
import           Data.Text                             (Text)
import           Data.Text.Encoding                    (encodeUtf8)
import           Data.Aeson                            (Value(..), toJSON, ToJSON, (.=), object)
import           Data.Generics.Product                 (getField)
import           Data.Scientific                       (toRealFloat, fromFloatDigits)
import qualified Data.HashSet as                       HS
import qualified Network.HTTP.Client as                Http
import           GHC.Generics                          (Generic)
import           GHC.Natural                           (Natural)
import           Network.HTTP.Client                   (newManager)
import           Network.HTTP.Client.TLS               (tlsManagerSettings)
import           System.Clock                          (TimeSpec(..))

import           LaunchDarkly.Server.Client.Internal          (Client(..), ClientI(..), getStatusI, clientVersion)
import           LaunchDarkly.Server.Client.Status            (Status(..))
import           LaunchDarkly.Server.Context                  (Context(..), getValue, redactContext, getKey, getKeys)
import           LaunchDarkly.Server.Config.ClientContext     (ClientContext(..))
import           LaunchDarkly.Server.Config.HttpConfiguration (HttpConfiguration(..))
import           LaunchDarkly.Server.Config.Internal          (ConfigI, Config(..), shouldSendEvents)
import           LaunchDarkly.Server.DataSource.Internal      (DataSource(..), DataSourceFactory, DataSourceUpdates(..), defaultDataSourceUpdates, nullDataSourceFactory)
import           LaunchDarkly.Server.Details                  (EvaluationDetail(..), EvaluationReason(..), EvalErrorKind(..))
import           LaunchDarkly.Server.Evaluate                 (evaluateTyped, evaluateDetail)
import           LaunchDarkly.Server.Events                   (IdentifyEvent(..), CustomEvent(..), EventType(..), makeBaseEvent, queueEvent, makeEventState, maybeIndexUser, unixMilliseconds, noticeUser)
import           LaunchDarkly.Server.Features                 (isClientSideOnlyFlag, isInExperiment)
import           LaunchDarkly.Server.Network.Eventing         (eventThread)
import           LaunchDarkly.Server.Network.Polling          (pollingThread)
import           LaunchDarkly.Server.Network.Streaming        (streamingThread)
import           LaunchDarkly.Server.Store.Internal           (makeStoreIO, getAllFlagsC)
import           LaunchDarkly.AesonCompat                     (KeyMap, insertKey, emptyObject, mapValues, filterObject)


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

    pure $ DataSource{..}

makeHttpConfiguration :: ConfigI -> IO HttpConfiguration
makeHttpConfiguration config = do
    tlsManager <- newManager tlsManagerSettings
    let defaultRequestHeaders = [ ("Authorization", encodeUtf8 $ getField @"key" config)
                                , ("User-Agent" , "HaskellServerClient/" <> encodeUtf8 clientVersion)
                                ]
        defaultRequestTimeout = Http.responseTimeoutMicro $ fromIntegral $ getField @"requestTimeoutSeconds" config * 1000000
    pure $ HttpConfiguration{..}

makeClientContext :: ConfigI -> IO ClientContext
makeClientContext config = do
    let runLogger = getField @"logger" config
    httpConfiguration <- makeHttpConfiguration config
    pure $ ClientContext{..}

-- | Create a new instance of the LaunchDarkly client.
makeClient :: Config -> IO Client
makeClient (Config config) = mfix $ \(Client client) -> do
    status  <- newIORef Uninitialized
    store   <- makeStoreIO (getField @"storeBackend" config) (TimeSpec (fromIntegral $ getField @"storeTTLSeconds" config) 0)
    manager <- case getField @"manager" config of
      Just manager -> pure manager
      Nothing      -> newManager tlsManagerSettings
    events  <- makeEventState config

    clientContext <- makeClientContext config

    let dataSourceUpdates = defaultDataSourceUpdates status store
    dataSource <- dataSourceFactory config clientContext dataSourceUpdates
    eventThreadPair    <- if not (shouldSendEvents config) then pure Nothing else do
        sync   <- newEmptyMVar
        thread <- forkFinally (runLogger clientContext $ eventThread manager client) (\_ -> putMVar sync ())
        pure $ pure (thread, sync)

    dataSourceStart dataSource

    pure $ Client $ ClientI{..}


dataSourceFactory :: ConfigI -> DataSourceFactory
dataSourceFactory config =
    if getField @"offline" config || getField @"useLdd" config then
        nullDataSourceFactory
    else
        case getField @"dataSourceFactory" config of
            Just factory ->
                factory
            Nothing ->
                let dataSourceThread =
                        if getField @"streaming" config then
                            streamingThread (getField @"streamURI" config)
                        else
                            pollingThread (getField @"baseURI" config) (getField @"pollIntervalSeconds" config)
                in networkDataSourceFactory dataSourceThread

clientRunLogger :: ClientI -> (LoggingT IO () -> IO ())
clientRunLogger client = getField @"logger" $ getField @"config" client

-- | Return the initialization status of the Client
getStatus :: Client -> IO Status
getStatus (Client client) = getStatusI client

-- TODO(mmk) This method exists in multiple places. Should we move this into a util file?
fromObject :: Value -> KeyMap Value
fromObject x = case x of (Object o) -> o; _ -> error "expected object"

-- | AllFlagsState captures the state of all feature flag keys as evaluated for
-- a specific user. This includes their values, as well as other metadata.
data AllFlagsState = AllFlagsState
    { evaluations :: !(KeyMap Value)
    , state :: !(KeyMap FlagState)
    , valid :: !Bool
    } deriving (Show, Generic)

instance ToJSON AllFlagsState where
    toJSON state = Object $
        insertKey "$flagsState" (toJSON $ getField @"state" state) $
        insertKey "$valid" (toJSON $ getField @"valid" state)
        (fromObject $ toJSON $ getField @"evaluations" state)

data FlagState = FlagState
    { version :: !(Maybe Natural)
    , variation :: !(Maybe Integer)
    , reason :: !(Maybe EvaluationReason)
    , trackEvents :: !Bool
    , trackReason :: !Bool
    , debugEventsUntilDate :: !(Maybe Natural)
    } deriving (Show, Generic)

instance ToJSON FlagState where
    toJSON state = object $
        filter ((/=) Null . snd)
        [ "version" .= getField @"version" state
        , "variation" .= getField @"variation" state
        , "trackEvents" .= if getField @"trackEvents" state then Just True else Nothing
        , "trackReason" .= if getField @"trackReason" state then Just True else Nothing
        , "reason" .= getField @"reason" state
        , "debugEventsUntilDate" .= getField @"debugEventsUntilDate" state
        ]

-- | Returns an object that encapsulates the state of all feature flags for a
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
allFlagsState (Client client) context client_side_only with_reasons details_only_for_tracked_flags  = do
    status <- getAllFlagsC $ getField @"store" client
    case status of
        Left _      -> pure AllFlagsState { evaluations = emptyObject, state = emptyObject, valid = False }
        Right flags -> do
            filtered <- pure $ (filterObject (\flag -> (not client_side_only) || isClientSideOnlyFlag flag) flags)
            details <- mapM (\flag -> (\detail -> (flag, fst detail)) <$> (evaluateDetail flag context HS.empty $ getField @"store" client)) filtered
            evaluations <- pure $ mapValues (getField @"value" . snd) details
            now <- unixMilliseconds
            state <- pure $ mapValues (\(flag, detail) -> do
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
                    }) details
            pure $ AllFlagsState { evaluations = evaluations, state = state, valid = True }

-- | Identify reports details about a context.
identify :: Client -> Context -> IO ()
identify (Client client) (Invalid err) = clientRunLogger client $ $(logWarn) $ "identify called with an invalid context: " <> err
identify (Client client) context = case (getValue "key" context) of
        (String "") -> clientRunLogger client $ $(logWarn) "identify called with empty key"
        _ -> do
            let redacted = redactContext (getField @"config" client) context
            x <- makeBaseEvent $ IdentifyEvent { key = getKey context, context = redacted }
            _ <- noticeUser (getField @"events" client) context
            queueEvent (getField @"config" client) (getField @"events" client) (EventTypeIdentify x)

-- | Track reports that a context has performed an event. Custom data can be
-- attached to the event, and / or a numeric value.
--
-- The numeric value is used by the LaunchDarkly experimentation feature in
-- numeric custom metrics, and will also be returned as part of the custom event
-- for Data Export.
track :: Client -> Context -> Text -> Maybe Value -> Maybe Double -> IO ()
track (Client client) (Invalid err) _ _ _ = clientRunLogger client $ $(logWarn) $ "track called with invalid context: " <> err
track (Client client) context key value metric = do
    x <- makeBaseEvent $ CustomEvent
        { key         = key
        , contextKeys = getKeys context
        , metricValue = metric
        , value       = value
        }
    let config = (getField @"config" client)
        events = (getField @"events" client)
    queueEvent config events (EventTypeCustom x)
    unixMilliseconds >>= \now -> maybeIndexUser now config context events

-- | Flush tells the client that all pending analytics events (if any) should be
-- delivered as soon as possible. Flushing is asynchronous, so this method will
-- return before it is complete.
flushEvents :: Client -> IO ()
flushEvents (Client client) = putMVar (getField @"flush" $ getField @"events" client) ()

-- | Close shuts down the LaunchDarkly client. After calling this, the
-- LaunchDarkly client should no longer be used. The method will block until all
-- pending analytics events have been sent.
close :: Client -> IO ()
close outer@(Client client) = clientRunLogger client $ do
    $(logDebug) "Setting client status to ShuttingDown"
    liftIO $ writeIORef (getField @"status" client) ShuttingDown
    liftIO $ dataSourceStop $ getField @"dataSource" client
    forM_ (getField @"eventThreadPair" client) $ \(_, sync) -> do
        $(logDebug) "Triggering event flush"
        liftIO $ flushEvents outer
        $(logDebug) "Waiting on event thread to die"
        liftIO $ void $ takeMVar sync
    $(logDebug) "Client background resources destroyed"

type ValueConverter a = (a -> Value, Value -> Maybe a)

reorderStuff :: ValueConverter a -> Bool -> Client -> Text -> Context -> a -> IO (EvaluationDetail a)
reorderStuff converter includeReason (Client client) key context fallback = evaluateTyped client key context fallback (fst converter) includeReason (snd converter)

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

-- | Evaluate a Number typed flag, truncate the result, and return an
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
