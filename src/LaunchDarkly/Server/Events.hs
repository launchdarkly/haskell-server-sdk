module LaunchDarkly.Server.Events where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, readMVar, swapMVar)
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
import LaunchDarkly.Server.Context.Internal (Context (Invalid), getCanonicalKey, getKinds, optionallyRedactAnonymous, redactContext, redactContextRedactAnonymous)
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
    , summary :: !(MVar SummaryState)
    , contextKeyLRU :: !(MVar (LRU Text ()))
    }
    deriving (Generic)

makeEventState :: Config -> IO EventState
makeEventState config = do
    events <- newMVar []
    lastKnownServerTime <- newMVar 0
    flush <- newEmptyMVar
    summary <- newMVar emptySummaryState
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

-- The summary data for the current flush window.
--
-- The start date and the counters are in one value. Thus a single MVar
-- operation changes both together. The start date is Nothing only when
-- there are no counters.
data SummaryState = SummaryState !(Maybe Natural) !(KeyMap FlagSummaryContext)

emptySummaryState :: SummaryState
emptySummaryState = SummaryState Nothing mempty

processSummary :: Config -> EventState -> IO ()
processSummary config state =
    swapMVar (getField @"summary" state) emptySummaryState >>= \case
        SummaryState Nothing _ -> pure ()
        SummaryState (Just startDate) features -> do
            endDate <- unixMilliseconds
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
    { defaultValue :: !(Maybe Value)
    , counters :: !(KeyMap CounterContext)
    , contextKinds :: !(HS.HashSet Text)
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

makeDebugEvent :: Config -> Context -> Bool -> EvalEvent -> DebugEvent
makeDebugEvent config context includeReason event =
    DebugEvent $ makeFeatureEventWithContextPayload (redactContext config context) includeReason event

makeFeatureEvent :: Config -> Context -> Bool -> EvalEvent -> FeatureEvent
makeFeatureEvent config context includeReason event =
    makeFeatureEventWithContextPayload (redactContextRedactAnonymous config context) includeReason event

makeFeatureEventWithContextPayload :: Value -> Bool -> EvalEvent -> FeatureEvent
makeFeatureEventWithContextPayload context includeReason event =
    FeatureEvent
        { key = getField @"key" event
        , context = context
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
    , context :: !Value
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
                , ("context", toJSON $ getField @"context" ctx)
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
    root' = root & field @"counters" %~ (insertKey key leaf) & field @"contextKinds" %~ (HS.union contextKinds)
    -- The map does not evaluate its values. Evaluate them here, so that
    -- repeated updates do not keep a chain of unevaluated values in memory.
    result = leaf `seq` root' `seq` insertKey (getField @"key" event) root' summaryContext

runSummary :: Natural -> EventState -> EvalEvent -> Bool -> IO ()
runSummary now state event unknown =
    modifyMVar_ (getField @"summary" state) $ \(SummaryState startDate features) ->
        -- Callers read the time before they get the MVar. Thus a later caller
        -- can write first. Keep the earliest time as the start date. Evaluate
        -- it now, so that repeated updates do not keep a chain of
        -- unevaluated values in memory.
        pure $! SummaryState (Just $! maybe now (min now) startDate) (summarizeEvent features event unknown)

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
                    makeDebugEvent config context includeReason event
    runSummary now state event unknown
    maybeIndexContext now config context state

processEvalEvents :: Config -> EventState -> Context -> Bool -> [EvalEvent] -> Bool -> IO ()
processEvalEvents config state context includeReason events unknown =
    -- Do not record any event data when the SDK does not send events.
    -- Otherwise the summary data grows without limit.
    when (shouldSendEvents config) $
        unixMilliseconds >>= \now -> mapM_ (processEvalEvent now config state context includeReason unknown) events

maybeIndexContext :: Natural -> Config -> Context -> EventState -> IO ()
maybeIndexContext now config context state = do
    case optionallyRedactAnonymous config context of
        (Invalid _) -> pure ()
        ctx -> do
            noticedContext <- noticeContext state ctx
            when noticedContext $
                queueEvent config state (EventTypeIndex $ BaseEvent now $ IndexEvent {context = redactContext config ctx})

noticeContext :: EventState -> Context -> IO Bool
noticeContext state context = modifyMVar (getField @"contextKeyLRU" state) $ \cache -> do
    let key = getCanonicalKey context
    case LRU.lookup key cache of
        (cache', Just _) -> pure (cache', False)
        (cache', Nothing) -> pure (LRU.insert key () cache', True)
