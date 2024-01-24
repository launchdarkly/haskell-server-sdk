module LaunchDarkly.Server.Events where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, swapMVar, tryTakeMVar)
import Control.Lens ((%~), (&))
import Control.Monad (when)
import Data.Aeson (ToJSON, Value (..), object, toJSON, (.=))
import Data.Cache.LRU (LRU, newLRU)
import qualified Data.Cache.LRU as LRU
import Data.Generics.Product (field, getField)
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Exts (fromList)
import GHC.Generics (Generic)
import GHC.Natural (Natural, naturalFromInteger)

import LaunchDarkly.AesonCompat (KeyMap, insertKey, keyMapUnion, lookupKey, objectValues)
import LaunchDarkly.Server.Config.Internal (Config, shouldSendEvents)
import LaunchDarkly.Server.Context (Context)
import LaunchDarkly.Server.Context.Internal (getCanonicalKey, getKinds, redactContext)
import LaunchDarkly.Server.Details (EvaluationReason (..))
import LaunchDarkly.Server.Features (Flag)

data EvalEvent = EvalEvent
    { key :: !Text
    , context :: !Context
    , variation :: !(Maybe Integer)
    , value :: !Value
    , defaultValue :: !(Maybe Value)
    , version :: !(Maybe Natural)
    , prereqOf :: !(Maybe Text)
    , reason :: !EvaluationReason
    , trackEvents :: !Bool
    , forceIncludeReason :: !Bool
    , debug :: !Bool
    , debugEventsUntilDate :: !(Maybe Natural)
    }
    deriving (Generic, Eq, Show)

data EventState = EventState
    { events :: !(MVar [EventType])
    , lastKnownServerTime :: !(MVar Integer)
    , flush :: !(MVar ())
    , summary :: !(MVar (KeyMap FlagSummaryContext))
    , startDate :: !(MVar Natural)
    , contextKeyLRU :: !(MVar (LRU Text ()))
    }
    deriving (Generic)

makeEventState :: Config -> IO EventState
makeEventState config = do
    events <- newMVar []
    lastKnownServerTime <- newMVar 0
    flush <- newEmptyMVar
    summary <- newMVar mempty
    startDate <- newEmptyMVar
    contextKeyLRU <- newMVar $ newLRU $ pure $ fromIntegral $ getField @"contextKeyLRUCapacity" config
    pure EventState {..}

queueEvent :: Config -> EventState -> EventType -> IO ()
queueEvent config state event =
    if not (shouldSendEvents config)
        then pure ()
        else modifyMVar_ (getField @"events" state) $ \events ->
            pure $ case event of
                EventTypeSummary _ -> event : events
                _ | length events < fromIntegral (getField @"eventsCapacity" config) -> event : events
                _ -> events

unixMilliseconds :: IO Natural
unixMilliseconds = round . (* 1000) <$> getPOSIXTime

makeBaseEvent :: a -> IO (BaseEvent a)
makeBaseEvent child = unixMilliseconds >>= \now -> pure $ BaseEvent {creationDate = now, event = child}

processSummary :: Config -> EventState -> IO ()
processSummary config state =
    tryTakeMVar (getField @"startDate" state) >>= \case
        Nothing -> pure ()
        (Just startDate) -> do
            endDate <- unixMilliseconds
            features <- swapMVar (getField @"summary" state) mempty
            queueEvent config state $ EventTypeSummary $ SummaryEvent {..}

class EventKind a where
    eventKind :: a -> Text

data SummaryEvent = SummaryEvent
    { startDate :: !Natural
    , endDate :: !Natural
    , features :: !(KeyMap FlagSummaryContext)
    }
    deriving (Generic, Show, ToJSON)

instance EventKind SummaryEvent where
    eventKind _ = "summary"

data FlagSummaryContext = FlagSummaryContext
    { defaultValue :: Maybe Value
    , counters :: KeyMap CounterContext
    , contextKinds :: HS.HashSet Text
    }
    deriving (Generic, Show)

instance ToJSON FlagSummaryContext where
    toJSON ctx =
        object $
            filter
                ((/=) Null . snd)
                [ ("default", toJSON $ getField @"defaultValue" ctx)
                , ("counters", toJSON $ objectValues $ getField @"counters" ctx)
                , ("contextKinds", toJSON $ getField @"contextKinds" ctx)
                ]

data CounterContext = CounterContext
    { count :: !Natural
    , version :: !(Maybe Natural)
    , variation :: !(Maybe Integer)
    , value :: !Value
    , unknown :: !Bool
    }
    deriving (Generic, Show)

instance ToJSON CounterContext where
    toJSON context =
        object $
            [ "count" .= getField @"count" context
            , "value" .= getField @"value" context
            ]
                <> filter
                    ((/=) Null . snd)
                    [ "version" .= getField @"version" context
                    , "variation" .= getField @"variation" context
                    , "unknown" .= if getField @"unknown" context then Just True else Nothing
                    ]

data IdentifyEvent = IdentifyEvent
    { key :: !Text
    , context :: !Value
    }
    deriving (Generic, ToJSON, Show)

instance EventKind IdentifyEvent where
    eventKind _ = "identify"

data IndexEvent = IndexEvent {context :: Value} deriving (Generic, ToJSON, Show)

instance EventKind IndexEvent where
    eventKind _ = "index"

data FeatureEvent = FeatureEvent
    { key :: !Text
    , context :: !Value
    , value :: !Value
    , defaultValue :: !(Maybe Value)
    , version :: !(Maybe Natural)
    , prereqOf :: !(Maybe Text)
    , variation :: !(Maybe Integer)
    , reason :: !(Maybe EvaluationReason)
    }
    deriving (Generic, Show)

instance ToJSON FeatureEvent where
    toJSON event =
        object $
            filter
                ((/=) Null . snd)
                [ ("key", toJSON $ getField @"key" event)
                , ("context", toJSON $ getField @"context" event)
                , ("value", toJSON $ getField @"value" event)
                , ("default", toJSON $ getField @"defaultValue" event)
                , ("version", toJSON $ getField @"version" event)
                , ("prereqOf", toJSON $ getField @"prereqOf" event)
                , ("variation", toJSON $ getField @"variation" event)
                , ("reason", toJSON $ getField @"reason" event)
                ]

instance EventKind FeatureEvent where
    eventKind _ = "feature"

newtype DebugEvent = DebugEvent FeatureEvent

instance EventKind DebugEvent where
    eventKind _ = "debug"

instance ToJSON DebugEvent where
    toJSON (DebugEvent x) = toJSON x

makeFeatureEvent :: Config -> Context -> Bool -> EvalEvent -> FeatureEvent
makeFeatureEvent config context includeReason event =
    FeatureEvent
        { key = getField @"key" event
        , context = redactContext config context
        , value = getField @"value" event
        , defaultValue = getField @"defaultValue" event
        , version = getField @"version" event
        , prereqOf = getField @"prereqOf" event
        , variation = getField @"variation" event
        , reason =
            if includeReason || getField @"forceIncludeReason" event
                then pure $ getField @"reason" event
                else Nothing
        }

data CustomEvent = CustomEvent
    { key :: !Text
    , contextKeys :: !(KeyMap Text)
    , metricValue :: !(Maybe Double)
    , value :: !(Maybe Value)
    }
    deriving (Generic, Show)

instance ToJSON CustomEvent where
    toJSON ctx =
        object $
            filter
                ((/=) Null . snd)
                [ ("key", toJSON $ getField @"key" ctx)
                , ("contextKeys", toJSON $ getField @"contextKeys" ctx)
                , ("metricValue", toJSON $ getField @"metricValue" ctx)
                , ("data", toJSON $ getField @"value" ctx)
                ]

instance EventKind CustomEvent where
    eventKind _ = "custom"

data BaseEvent event = BaseEvent
    { creationDate :: Natural
    , event :: event
    }
    deriving (Generic, Show)

fromObject :: Value -> KeyMap Value
fromObject x = case x of (Object o) -> o; _ -> error "expected object"

instance (EventKind sub, ToJSON sub) => ToJSON (BaseEvent sub) where
    toJSON event =
        Object $
            keyMapUnion (fromObject $ toJSON $ getField @"event" event) $
                fromList
                    [ ("creationDate", toJSON $ getField @"creationDate" event)
                    , ("kind", String $ eventKind $ getField @"event" event)
                    ]

data EventType
    = EventTypeIdentify !(BaseEvent IdentifyEvent)
    | EventTypeFeature !(BaseEvent FeatureEvent)
    | EventTypeSummary !SummaryEvent
    | EventTypeCustom !(BaseEvent CustomEvent)
    | EventTypeIndex !(BaseEvent IndexEvent)
    | EventTypeDebug !(BaseEvent DebugEvent)

instance ToJSON EventType where
    toJSON event = case event of
        EventTypeIdentify x -> toJSON x
        EventTypeFeature x -> toJSON x
        EventTypeSummary x -> Object $ insertKey "kind" (String "summary") (fromObject $ toJSON x)
        EventTypeCustom x -> toJSON x
        EventTypeIndex x -> toJSON x
        EventTypeDebug x -> toJSON x

newUnknownFlagEvent :: Text -> Value -> EvaluationReason -> Context -> EvalEvent
newUnknownFlagEvent key defaultValue reason context =
    EvalEvent
        { key = key
        , context = context
        , variation = Nothing
        , value = defaultValue
        , defaultValue = pure defaultValue
        , version = Nothing
        , prereqOf = Nothing
        , reason = reason
        , trackEvents = False
        , forceIncludeReason = False
        , debug = False
        , debugEventsUntilDate = Nothing
        }

newSuccessfulEvalEvent :: Flag -> Maybe Integer -> Value -> Maybe Value -> EvaluationReason -> Maybe Text -> Context -> EvalEvent
newSuccessfulEvalEvent flag variation value defaultValue reason prereqOf context =
    EvalEvent
        { key = getField @"key" flag
        , context = context
        , variation = variation
        , value = value
        , defaultValue = defaultValue
        , version = Just $ getField @"version" flag
        , prereqOf = prereqOf
        , reason = reason
        , trackEvents = getField @"trackEvents" flag || shouldForceReason
        , forceIncludeReason = shouldForceReason
        , debug = False
        , debugEventsUntilDate = getField @"debugEventsUntilDate" flag
        }
  where
    shouldForceReason = case reason of
        (EvaluationReasonFallthrough inExperiment) ->
            inExperiment || getField @"trackEventsFallthrough" flag
        (EvaluationReasonRuleMatch idx _ inExperiment) ->
            inExperiment || getField @"trackEvents" (getField @"rules" flag !! fromIntegral idx)
        _ -> False

makeSummaryKey :: EvalEvent -> Text
makeSummaryKey event =
    T.intercalate
        "-"
        [ fromMaybe "" $ fmap (T.pack . show) $ getField @"version" event
        , fromMaybe "" $ fmap (T.pack . show) $ getField @"variation" event
        ]

summarizeEvent :: KeyMap FlagSummaryContext -> EvalEvent -> Bool -> KeyMap FlagSummaryContext
summarizeEvent summaryContext event unknown = result
  where
    key = makeSummaryKey event
    contextKinds = HS.fromList $ getKinds $ getField @"context" event
    root = case lookupKey (getField @"key" event) summaryContext of
        (Just x) -> x
        Nothing ->
            FlagSummaryContext
                { defaultValue = (getField @"defaultValue" event)
                , counters = mempty
                , contextKinds = mempty
                }
    leaf = case lookupKey key (getField @"counters" root) of
        (Just x) -> x & field @"count" %~ (1 +)
        Nothing ->
            CounterContext
                { count = 1
                , version = getField @"version" event
                , variation = getField @"variation" event
                , value = getField @"value" event
                , unknown = unknown
                }
    result = flip (insertKey $ getField @"key" event) summaryContext $ (root & field @"counters" %~ (insertKey key leaf) & field @"contextKinds" %~ (HS.union contextKinds))

putIfEmptyMVar :: MVar a -> a -> IO ()
putIfEmptyMVar mvar value = tryTakeMVar mvar >>= \case Just x -> putMVar mvar x; Nothing -> putMVar mvar value

runSummary :: Natural -> EventState -> EvalEvent -> Bool -> IO ()
runSummary now state event unknown =
    putIfEmptyMVar (getField @"startDate" state) now
        >> modifyMVar_ (getField @"summary" state) (\summary -> pure $ summarizeEvent summary event unknown)

processEvalEvent :: Natural -> Config -> EventState -> Context -> Bool -> Bool -> EvalEvent -> IO ()
processEvalEvent now config state context includeReason unknown event = do
    let featureEvent = makeFeatureEvent config context includeReason event
        trackEvents = getField @"trackEvents" event
        debugEventsUntilDate = fromMaybe 0 (getField @"debugEventsUntilDate" event)
    lastKnownServerTime <- naturalFromInteger <$> (* 1000) <$> readMVar (getField @"lastKnownServerTime" state)
    when trackEvents $
        queueEvent config state $
            EventTypeFeature $
                BaseEvent now featureEvent
    when (now < debugEventsUntilDate && lastKnownServerTime < debugEventsUntilDate) $
        queueEvent config state $
            EventTypeDebug $
                BaseEvent now $
                    DebugEvent featureEvent
    runSummary now state event unknown
    maybeIndexContext now config context state

processEvalEvents :: Config -> EventState -> Context -> Bool -> [EvalEvent] -> Bool -> IO ()
processEvalEvents config state context includeReason events unknown =
    unixMilliseconds >>= \now -> mapM_ (processEvalEvent now config state context includeReason unknown) events

maybeIndexContext :: Natural -> Config -> Context -> EventState -> IO ()
maybeIndexContext now config context state = do
    noticedContext <- noticeContext state context
    when noticedContext $
        queueEvent config state (EventTypeIndex $ BaseEvent now $ IndexEvent {context = redactContext config context})

noticeContext :: EventState -> Context -> IO Bool
noticeContext state context = modifyMVar (getField @"contextKeyLRU" state) $ \cache -> do
    let key = getCanonicalKey context
    case LRU.lookup key cache of
        (cache', Just _) -> pure (cache', False)
        (cache', Nothing) -> pure (LRU.insert key () cache', True)
