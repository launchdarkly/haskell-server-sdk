module LaunchDarkly.Server.Events where

import           Data.Aeson                          (ToJSON, Value(..), toJSON, object, (.=))
import           Data.Text                           (Text)
import           GHC.Exts                            (fromList)
import           GHC.Natural                         (Natural, naturalFromInteger)
import           GHC.Generics                        (Generic)
import           Data.Generics.Product               (HasField', getField, field, setField)
import qualified Data.Text as                        T
import           Control.Concurrent.MVar             (MVar, putMVar, swapMVar, newEmptyMVar, newMVar, tryTakeMVar, modifyMVar_, modifyMVar, readMVar)
import           Data.Time.Clock.POSIX               (getPOSIXTime)
import           Control.Lens                        ((&), (%~))
import           Data.Maybe                          (fromMaybe)
import           Data.Cache.LRU                      (LRU, newLRU)
import           Control.Monad                       (when, unless)
import qualified Data.Cache.LRU as                   LRU

import           LaunchDarkly.AesonCompat            (KeyMap, keyMapUnion, insertKey, mapValues, objectValues, lookupKey)
import           LaunchDarkly.Server.Config.Internal (ConfigI, shouldSendEvents)
import           LaunchDarkly.Server.User.Internal   (UserI, userSerializeRedacted)
import           LaunchDarkly.Server.Details         (EvaluationReason(..))
import           LaunchDarkly.Server.Features        (Flag)

data ContextKind = ContextKindUser | ContextKindAnonymousUser
    deriving (Eq, Show)

instance ToJSON ContextKind where
  toJSON contextKind = String $ case contextKind of
      ContextKindUser          -> "user"
      ContextKindAnonymousUser -> "anonymousUser"

userGetContextKind :: UserI -> ContextKind
userGetContextKind user = if getField @"anonymous" user
    then ContextKindAnonymousUser else ContextKindUser

data EvalEvent = EvalEvent
    { key                  :: !Text
    , variation            :: !(Maybe Integer)
    , value                :: !Value
    , defaultValue         :: !(Maybe Value)
    , version              :: !(Maybe Natural)
    , prereqOf             :: !(Maybe Text)
    , reason               :: !EvaluationReason
    , trackEvents          :: !Bool
    , forceIncludeReason   :: !Bool
    , debug                :: !Bool
    , debugEventsUntilDate :: !(Maybe Natural)
    } deriving (Generic, Eq, Show)

data EventState = EventState
    { events              :: !(MVar [EventType])
    , lastKnownServerTime :: !(MVar Integer)
    , flush               :: !(MVar ())
    , summary             :: !(MVar (KeyMap (FlagSummaryContext (KeyMap CounterContext))))
    , startDate           :: !(MVar Natural)
    , userKeyLRU          :: !(MVar (LRU Text ()))
    } deriving (Generic)

makeEventState :: ConfigI -> IO EventState
makeEventState config = do
    events              <- newMVar []
    lastKnownServerTime <- newMVar 0
    flush               <- newEmptyMVar
    summary             <- newMVar mempty
    startDate           <- newEmptyMVar
    userKeyLRU          <- newMVar $ newLRU $ pure $ fromIntegral $ getField @"userKeyLRUCapacity" config
    pure EventState{..}

convertFeatures :: KeyMap (FlagSummaryContext (KeyMap CounterContext))
    -> KeyMap (FlagSummaryContext [CounterContext])
convertFeatures summary = flip mapValues summary $ \context -> context & field @"counters" %~ objectValues

queueEvent :: ConfigI -> EventState -> EventType -> IO ()
queueEvent config state event = if not (shouldSendEvents config) then pure () else
    modifyMVar_ (getField @"events" state) $ \events ->
        pure $ case event of
          EventTypeSummary _ -> event : events
          _ | length events < fromIntegral (getField @"eventsCapacity" config) -> event : events
          _ -> events

unixMilliseconds :: IO Natural
unixMilliseconds = round . (* 1000) <$> getPOSIXTime

makeBaseEvent :: a -> IO (BaseEvent a)
makeBaseEvent child = unixMilliseconds >>= \now -> pure $ BaseEvent { creationDate = now, event = child }

processSummary :: ConfigI -> EventState -> IO ()
processSummary config state = tryTakeMVar (getField @"startDate" state) >>= \case
    Nothing          -> pure ()
    (Just startDate) -> do
        endDate  <- unixMilliseconds
        features <- convertFeatures <$> swapMVar (getField @"summary" state) mempty
        queueEvent config state $ EventTypeSummary $ SummaryEvent {..}

class EventKind a where
    eventKind :: a -> Text

data SummaryEvent = SummaryEvent
    { startDate :: !Natural
    , endDate   :: !Natural
    , features  :: !(KeyMap (FlagSummaryContext [CounterContext]))
    } deriving (Generic, Show, ToJSON)

instance EventKind SummaryEvent where
    eventKind _ = "summary"

data FlagSummaryContext a = FlagSummaryContext
    { defaultValue :: Maybe Value
    , counters     :: a
    } deriving (Generic, Show)

instance ToJSON a => ToJSON (FlagSummaryContext a) where
    toJSON ctx = object $ filter ((/=) Null . snd)
        [ ("default",  toJSON $ getField @"defaultValue" ctx)
        , ("counters", toJSON $ getField @"counters"     ctx)
        ]

data CounterContext = CounterContext
    { count     :: !Natural
    , version   :: !(Maybe Natural)
    , variation :: !(Maybe Integer)
    , value     :: !Value
    , unknown   :: !Bool
    } deriving (Generic, Show)

instance ToJSON CounterContext where
    toJSON context = object $
        [ "count" .= getField @"count" context
        , "value" .= getField @"value" context
        ] <> filter ((/=) Null . snd)
        [ "version" .= getField @"version" context
        , "variation" .= getField @"variation" context
        , "unknown" .= if getField @"unknown" context then Just True else Nothing
        ]

data IdentifyEvent = IdentifyEvent
    { key  :: !Text
    , user :: !Value
    } deriving (Generic, ToJSON, Show)

instance EventKind IdentifyEvent where
    eventKind _ = "identify"

data IndexEvent = IndexEvent { user :: Value } deriving (Generic, ToJSON, Show)

instance EventKind IndexEvent where
    eventKind _ = "index"

data FeatureEvent = FeatureEvent
    { key          :: !Text
    , user         :: !(Maybe Value)
    , userKey      :: !(Maybe Text)
    , value        :: !Value
    , defaultValue :: !(Maybe Value)
    , version      :: !(Maybe Natural)
    , variation    :: !(Maybe Integer)
    , reason       :: !(Maybe EvaluationReason)
    , contextKind  :: !ContextKind
    } deriving (Generic, Show)

instance ToJSON FeatureEvent where
    toJSON event = object $ filter ((/=) Null . snd)
        [ ("key",         toJSON $ getField @"key"          event)
        , ("user",        toJSON $ getField @"user"         event)
        , ("userKey",     toJSON $ getField @"userKey"      event)
        , ("value",       toJSON $ getField @"value"        event)
        , ("default",     toJSON $ getField @"defaultValue" event)
        , ("version",     toJSON $ getField @"version"      event)
        , ("variation",   toJSON $ getField @"variation"    event)
        , ("reason",      toJSON $ getField @"reason"       event)
        , ("contextKind", let c = getField @"contextKind" event in
            if c == ContextKindUser then Null else toJSON c)
        ]

instance EventKind FeatureEvent where
    eventKind _ = "feature"

newtype DebugEvent = DebugEvent FeatureEvent

instance EventKind DebugEvent where
    eventKind _ = "debug"

instance ToJSON DebugEvent where
    toJSON (DebugEvent x) = toJSON x

addUserToEvent :: (HasField' "user" r (Maybe Value), HasField' "userKey" r (Maybe Text)) => ConfigI -> UserI -> r -> r
addUserToEvent config user event = if getField @"inlineUsersInEvents" config
    then setField @"user" (pure $ userSerializeRedacted config user) event
    else setField @"userKey" (pure $ getField @"key" user) event

forceUserInlineInEvent :: ConfigI -> UserI -> FeatureEvent -> FeatureEvent
forceUserInlineInEvent config user event = setField @"userKey" Nothing $ setField @"user" (pure $ userSerializeRedacted config user) event

makeFeatureEvent :: ConfigI -> UserI -> Bool -> EvalEvent -> FeatureEvent
makeFeatureEvent config user includeReason event = addUserToEvent config user $ FeatureEvent
    { key          = getField @"key" event
    , user         = Nothing
    , userKey      = Nothing
    , value        = getField @"value" event
    , defaultValue = getField @"defaultValue" event
    , version      = getField @"version" event
    , variation    = getField @"variation" event
    , reason       = if includeReason || getField @"forceIncludeReason" event
        then pure $ getField @"reason" event else Nothing
    , contextKind  = userGetContextKind user
    }

data CustomEvent = CustomEvent
    { key         :: !Text
    , user        :: !(Maybe Value)
    , userKey     :: !(Maybe Text)
    , metricValue :: !(Maybe Double)
    , value       :: !(Maybe Value)
    , contextKind :: !ContextKind
    } deriving (Generic, Show)

instance ToJSON CustomEvent where
    toJSON ctx = object $ filter ((/=) Null . snd)
        [ ("key",         toJSON $ getField @"key"         ctx)
        , ("user",        toJSON $ getField @"user"        ctx)
        , ("userKey",     toJSON $ getField @"userKey"     ctx)
        , ("metricValue", toJSON $ getField @"metricValue" ctx)
        , ("data",        toJSON $ getField @"value"       ctx)
        , ("contextKind", let c = getField @"contextKind" ctx in
            if c == ContextKindUser then Null else toJSON c)
        ]

instance EventKind CustomEvent where
    eventKind _ = "custom"

data BaseEvent event = BaseEvent
    { creationDate :: Natural
    , event        :: event
    } deriving (Generic, Show)

fromObject :: Value -> KeyMap Value
fromObject x = case x of (Object o) -> o; _ -> error "expected object"

instance (EventKind sub, ToJSON sub) => ToJSON (BaseEvent sub) where
    toJSON event = Object $ keyMapUnion (fromObject $ toJSON $ getField @"event" event) $ fromList
        [ ("creationDate", toJSON $ getField @"creationDate" event)
        , ("kind",         String $ eventKind $ getField @"event" event)
        ]

data EventType =
      EventTypeIdentify !(BaseEvent IdentifyEvent)
    | EventTypeFeature  !(BaseEvent FeatureEvent)
    | EventTypeSummary  !SummaryEvent
    | EventTypeCustom   !(BaseEvent CustomEvent)
    | EventTypeIndex    !(BaseEvent IndexEvent)
    | EventTypeDebug    !(BaseEvent DebugEvent)

instance ToJSON EventType where
    toJSON event = case event of
        EventTypeIdentify x -> toJSON x
        EventTypeFeature  x -> toJSON x
        EventTypeSummary  x -> Object $ insertKey "kind" (String "summary") (fromObject $ toJSON x)
        EventTypeCustom   x -> toJSON x
        EventTypeIndex    x -> toJSON x
        EventTypeDebug    x -> toJSON x

newUnknownFlagEvent :: Text -> Value -> EvaluationReason -> EvalEvent
newUnknownFlagEvent key defaultValue reason = EvalEvent
    { key                  = key
    , variation            = Nothing
    , value                = defaultValue
    , defaultValue         = pure defaultValue
    , version              = Nothing
    , prereqOf             = Nothing
    , reason               = reason
    , trackEvents          = False
    , forceIncludeReason   = False
    , debug                = False
    , debugEventsUntilDate = Nothing
    }

newSuccessfulEvalEvent :: Flag -> Maybe Integer -> Value -> Maybe Value -> EvaluationReason -> Maybe Text -> EvalEvent
newSuccessfulEvalEvent flag variation value defaultValue reason prereqOf = EvalEvent
    { key                  = getField @"key" flag
    , variation            = variation
    , value                = value
    , defaultValue         = defaultValue
    , version              = Just $ getField @"version" flag
    , prereqOf             = prereqOf
    , reason               = reason
    , trackEvents          = getField @"trackEvents" flag || shouldForceReason
    , forceIncludeReason   = shouldForceReason
    , debug                = False
    , debugEventsUntilDate = getField @"debugEventsUntilDate" flag
    }

    where

    shouldForceReason = case reason of
        (EvaluationReasonFallthrough inExperiment)     ->
            inExperiment || getField @"trackEventsFallthrough" flag
        (EvaluationReasonRuleMatch idx _ inExperiment) ->
            inExperiment || getField @"trackEvents" (getField @"rules" flag !! fromIntegral idx)
        _                                              -> False

makeSummaryKey :: EvalEvent -> Text
makeSummaryKey event = T.intercalate "-"
    [ fromMaybe "" $ fmap (T.pack . show) $ getField @"version" event
    , fromMaybe "" $ fmap (T.pack . show) $ getField @"variation" event
    ]

summarizeEvent :: KeyMap (FlagSummaryContext (KeyMap CounterContext))
    -> EvalEvent -> Bool -> KeyMap (FlagSummaryContext (KeyMap CounterContext))
summarizeEvent context event unknown = result where
    key = makeSummaryKey event
    root = case lookupKey (getField @"key" event) context of
        (Just x) -> x; Nothing  -> FlagSummaryContext (getField @"defaultValue" event) mempty
    leaf = case lookupKey key (getField @"counters" root) of
        (Just x) -> x & field @"count" %~ (1 +)
        Nothing  -> CounterContext
            { count     = 1
            , version   = getField @"version" event
            , variation = getField @"variation" event
            , value     = getField @"value" event
            , unknown   = unknown
            }
    result = flip (insertKey $ getField @"key" event) context $
        root & field @"counters" %~ insertKey key leaf

putIfEmptyMVar :: MVar a -> a -> IO ()
putIfEmptyMVar mvar value = tryTakeMVar mvar >>= \case Just x -> putMVar mvar x; Nothing -> putMVar mvar value;

runSummary :: Natural -> EventState -> EvalEvent -> Bool -> IO ()
runSummary now state event unknown = putIfEmptyMVar (getField @"startDate" state) now >>
    modifyMVar_ (getField @"summary" state) (\summary -> pure $ summarizeEvent summary event unknown)

processEvalEvent :: Natural -> ConfigI -> EventState -> UserI -> Bool -> Bool -> EvalEvent -> IO ()
processEvalEvent now config state user includeReason unknown event = do
    let featureEvent = makeFeatureEvent config user includeReason event
        trackEvents = getField @"trackEvents" event
        inlineUsers = getField @"inlineUsersInEvents" config
        debugEventsUntilDate = fromMaybe 0 (getField @"debugEventsUntilDate" event)
    lastKnownServerTime <- naturalFromInteger <$> (* 1000) <$> readMVar (getField @"lastKnownServerTime" state)
    when trackEvents $
        queueEvent config state $ EventTypeFeature $ BaseEvent now featureEvent
    when (now < debugEventsUntilDate && lastKnownServerTime < debugEventsUntilDate) $
        queueEvent config state $ EventTypeDebug $ BaseEvent now $ DebugEvent $ forceUserInlineInEvent config user featureEvent
    runSummary now state event unknown
    unless (trackEvents && inlineUsers) $
        maybeIndexUser now config user state

processEvalEvents :: ConfigI -> EventState -> UserI -> Bool -> [EvalEvent] -> Bool -> IO ()
processEvalEvents config state user includeReason events unknown = unixMilliseconds >>= \now ->
    mapM_ (processEvalEvent now config state user includeReason unknown) events

maybeIndexUser :: Natural -> ConfigI -> UserI -> EventState -> IO ()
maybeIndexUser now config user state = do
    noticedUser <- noticeUser state user
    when noticedUser $
        queueEvent config state (EventTypeIndex $ BaseEvent now $ IndexEvent { user = userSerializeRedacted config user })

noticeUser :: EventState -> UserI -> IO Bool
noticeUser state user = modifyMVar (getField @"userKeyLRU" state) $ \cache -> do
    let key = getField @"key" user
    case LRU.lookup key cache of
        (cache', Just _)  -> pure (cache', False)
        (cache', Nothing) -> pure (LRU.insert key () cache', True)
