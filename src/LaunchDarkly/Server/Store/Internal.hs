module LaunchDarkly.Server.Store.Internal
    ( isInitialized
    , getAllFlags
    , getFlag
    , getSegment
    , upsertFlag
    , upsertSegment
    , initialize
    , StoreResult
    , StoreResultM
    , StoreInterface(..)
    , RawFeature(..)
    , StoreHandle(..)
    , LaunchDarklyStoreRead(..)
    , LaunchDarklyStoreWrite(..)
    , Versioned(..)
    , makeStoreIO
    , insertFlag
    , deleteFlag
    , insertSegment
    , deleteSegment
    , initializeStore
    , versionedToRaw
    , FeatureKey
    , FeatureNamespace
    ) where

import           Control.Monad                (void)
import           Data.Generics.Internal.VL    (Lens', (^.), over)
import           Data.Aeson                   (ToJSON, FromJSON, encode, decode)
import           Data.IORef                   (IORef, readIORef, atomicModifyIORef', newIORef)
import           Data.ByteString              (ByteString)
import           Data.ByteString.Lazy         (toStrict, fromStrict)
import           Data.Text                    (Text)
import           Data.Function                ((&))
import           Data.Maybe                   (isJust)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict as       HM
import           Data.Generics.Product        (setField, getField, field)
import           System.Clock                 (TimeSpec, Clock(Monotonic), getTime)
import           GHC.Generics                 (Generic)
import           GHC.Natural                  (Natural)

import           LaunchDarkly.Server.Features (Segment, Flag)

-- Store result not defined in terms of StoreResultM so we dont have to export.
type StoreResultM m a = m (Either Text a)

-- | The result type for every `StoreInterface` function. Instead of throwing
-- an exception, any store related error should return `Left`. Exceptions
-- unrelated to the store should not be caught.
type StoreResult a = IO (Either Text a)

class LaunchDarklyStoreRead store m where
    getFlagC        :: store -> Text -> StoreResultM m (Maybe Flag)
    getSegmentC     :: store -> Text -> StoreResultM m (Maybe Segment)
    getAllFlagsC    :: store -> StoreResultM m (HashMap Text Flag)
    getInitializedC :: store -> StoreResultM m Bool

class LaunchDarklyStoreWrite store m where
    storeInitializeC :: store -> HashMap Text (Versioned Flag) -> HashMap Text (Versioned Segment) -> StoreResultM m ()
    upsertSegmentC   :: store -> Text -> Versioned (Maybe Segment) -> StoreResultM m ()
    upsertFlagC      :: store -> Text -> Versioned (Maybe Flag) -> StoreResultM m ()

data StoreHandle m = StoreHandle
    { storeHandleGetFlag       :: !(Text -> StoreResultM m (Maybe Flag))
    , storeHandleGetSegment    :: !(Text -> StoreResultM m (Maybe Segment))
    , storeHandleAllFlags      :: !(StoreResultM m (HashMap Text Flag))
    , storeHandleInitialized   :: !(StoreResultM m Bool)
    , storeHandleInitialize    :: !(HashMap Text (Versioned Flag) -> HashMap Text (Versioned Segment) -> StoreResultM m ())
    , storeHandleUpsertSegment :: !(Text -> Versioned (Maybe Segment) -> StoreResultM m ())
    , storeHandleUpsertFlag    :: !(Text -> Versioned (Maybe Flag) -> StoreResultM m ())
    , storeHandleExpireAll     :: !(StoreResultM m ())
    } deriving (Generic)

instance Monad m => LaunchDarklyStoreRead (StoreHandle m) m where
    getFlagC        = storeHandleGetFlag
    getSegmentC     = storeHandleGetSegment
    getAllFlagsC    = storeHandleAllFlags
    getInitializedC = storeHandleInitialized

instance Monad m => LaunchDarklyStoreWrite (StoreHandle m) m where
    storeInitializeC = storeHandleInitialize
    upsertSegmentC   = storeHandleUpsertSegment
    upsertFlagC      = storeHandleUpsertFlag

initializeStore :: (LaunchDarklyStoreWrite store m, Monad m) => store
    -> HashMap Text Flag -> HashMap Text Segment -> StoreResultM m ()
initializeStore store flags segments = storeInitializeC store (makeVersioned flags) (makeVersioned segments)
    where makeVersioned = HM.map (\f -> Versioned f (getField @"version" f))

insertFlag :: (LaunchDarklyStoreWrite store m, Monad m) => store -> Flag -> StoreResultM m ()
insertFlag store flag = upsertFlagC store (getField @"key" flag) $ Versioned (pure flag) (getField @"version" flag)

deleteFlag :: (LaunchDarklyStoreWrite store m, Monad m) => store -> Text -> Natural -> StoreResultM m ()
deleteFlag store key version = upsertFlagC store key $ Versioned Nothing version

insertSegment :: (LaunchDarklyStoreWrite store m, Monad m) => store -> Segment -> StoreResultM m ()
insertSegment store segment = upsertSegmentC store (getField @"key" segment) $ Versioned (pure segment) (getField @"version" segment)

deleteSegment :: (LaunchDarklyStoreWrite store m, Monad m) => store -> Text -> Natural -> StoreResultM m ()
deleteSegment store key version = upsertSegmentC store key $ Versioned Nothing version

makeStoreIO :: Maybe StoreInterface -> TimeSpec -> IO (StoreHandle IO)
makeStoreIO backend ttl = do
    state <- newIORef State
        { allFlags    = Expirable HM.empty True 0
        , flags       = HM.empty
        , segments    = HM.empty
        , initialized = Expirable False True 0
        }
    let store = Store state backend ttl
    pure StoreHandle
        { storeHandleGetFlag       = getFlag        store
        , storeHandleGetSegment    = getSegment     store
        , storeHandleAllFlags      = getAllFlags    store
        , storeHandleInitialized   = isInitialized  store
        , storeHandleInitialize    = initialize     store
        , storeHandleUpsertSegment = upsertSegment  store
        , storeHandleUpsertFlag    = upsertFlag     store
        , storeHandleExpireAll     = expireAllItems store >> (pure $ Right ())
        }

data Expirable a = Expirable
    { value       :: !a
    , forceExpire :: !Bool
    , updatedOn   :: !TimeSpec
    } deriving (Generic)

data Versioned a = Versioned
    { value   :: !a
    , version :: !Natural
    } deriving (Generic)

data State = State
    { allFlags    :: !(Expirable (HashMap Text Flag))
    , flags       :: !(HashMap Text (Expirable (Versioned (Maybe Flag))))
    , segments    :: !(HashMap Text (Expirable (Versioned (Maybe Segment))))
    , initialized :: !(Expirable Bool)
    } deriving (Generic)

-- | Represents the key for a given feature.
type FeatureKey       = Text
-- | Represents a namespace such as flags or segments
type FeatureNamespace = Text

-- | The interface implemented by external stores for use by the SDK.
data StoreInterface = StoreInterface
    { storeInterfaceAllFeatures   :: !(FeatureNamespace -> StoreResult (HashMap Text RawFeature))
      -- ^ A map of all features in a given namespace including deleted.
    , storeInterfaceGetFeature    :: !(FeatureNamespace -> FeatureKey -> StoreResult RawFeature)
      -- ^ Return the value of a key in a namespace.
    , storeInterfaceUpsertFeature :: !(FeatureNamespace -> FeatureKey -> RawFeature -> StoreResult Bool)
      -- ^ Upsert a given feature. Versions should be compared before upsert.
      -- The result should indicate if the feature was replaced or not.
    , storeInterfaceIsInitialized :: !(StoreResult Bool)
      -- ^ Checks if the external store has been initialized, which may
      -- have been done by another instance of the SDK.
    , storeInterfaceInitialize    :: !(HashMap FeatureNamespace (HashMap FeatureKey RawFeature) -> StoreResult ())
      -- ^ A map of namespaces, and items in namespaces. The entire store state
      -- should be replaced with these values.
    }

-- | An abstract representation of a store object.
data RawFeature = RawFeature
    { rawFeatureBuffer  :: !(Maybe ByteString)
      -- ^ A serialized item. If the item is deleted or does not exist this
      -- should be `Nothing`.
    , rawFeatureVersion :: !Natural
      -- ^ The version of a given item. If the item does not exist this should
      -- be zero.
    }

data Store = Store
    { state      :: !(IORef State)
    , backend    :: !(Maybe StoreInterface)
    , timeToLive :: !TimeSpec
    } deriving (Generic)

expireAllItems :: Store -> IO ()
expireAllItems store = atomicModifyIORef' (getField @"state" store) $ \state -> (, ()) $ state
    & field @"allFlags"    `over` expire
    & field @"initialized" `over` expire
    & field @"flags"       `over` HM.map expire
    & field @"segments"    `over` HM.map expire
    where expire = setField @"forceExpire" True

isExpired :: Store -> TimeSpec -> Expirable a -> Bool
isExpired store now item = (isJust $ getField @"backend" store) && ((getField @"forceExpire" item)
    || (getField @"timeToLive" store) + (getField @"updatedOn" item) < now)

getMonotonicTime :: IO TimeSpec
getMonotonicTime = getTime Monotonic

initialize :: Store -> HashMap Text (Versioned Flag) -> HashMap Text (Versioned Segment) -> StoreResult ()
initialize store flags segments = case getField @"backend" store of
    Nothing      -> do
        atomicModifyIORef' (getField @"state" store) $ \state -> (, ()) $ state
            & setField @"flags"       (HM.map (\f -> Expirable f True 0) $ c flags)
            & setField @"segments"    (HM.map (\f -> Expirable f True 0) $ c segments)
            & setField @"allFlags"    (Expirable (HM.map (getField @"value") flags) True 0)
            & setField @"initialized" (Expirable True False 0)
        pure $ Right ()
    Just backend -> (storeInterfaceInitialize backend) raw >>= \case
        Left err -> pure $ Left err
        Right () -> expireAllItems store >> pure (Right ())
    where
        raw = HM.empty
            & HM.insert "flags"    (HM.map versionedToRaw $ c flags)
            & HM.insert "segments" (HM.map versionedToRaw $ c segments)
        c x = HM.map (\f -> f & field @"value" `over` Just) x

rawToVersioned :: (FromJSON a) => RawFeature -> Maybe (Versioned (Maybe a))
rawToVersioned raw = case rawFeatureBuffer raw of
    Nothing     -> pure $ Versioned Nothing (rawFeatureVersion raw)
    Just buffer -> case decode $ fromStrict buffer of
        Nothing      -> Nothing
        Just decoded -> pure $ Versioned decoded (rawFeatureVersion raw)

versionedToRaw :: (ToJSON a) => Versioned (Maybe a) -> RawFeature
versionedToRaw versioned = case getField @"value" versioned of
    Nothing -> RawFeature Nothing $ getField @"version" versioned
    Just x  -> RawFeature (pure $ toStrict $ encode x) $ getField @"version" versioned

tryGetBackend :: (FromJSON a) => StoreInterface -> Text -> Text -> StoreResult (Versioned (Maybe a))
tryGetBackend backend namespace key =
    ((storeInterfaceGetFeature backend) namespace key) >>= \case
        Left err  -> pure $ Left err
        Right raw -> case rawToVersioned raw of
            Nothing        -> pure $ Left "failed to decode from external store"
            Just versioned -> pure $ Right versioned

getGeneric :: FromJSON a => Store -> Text -> Text
    -> Lens' State (HashMap Text (Expirable (Versioned (Maybe a))))
    -> StoreResult (Maybe a)
getGeneric store namespace key lens = do
    state <- readIORef $ getField @"state" store
    case getField @"backend" store of
        Nothing      -> case HM.lookup key (state ^. lens) of
            Nothing -> pure $ Right Nothing
            Just x  -> pure $ Right $ getField @"value" $ getField @"value" x
        Just backend -> do
            now <- getMonotonicTime
            case HM.lookup key (state ^. lens) of
                Nothing -> updateFromBackend backend now
                Just x  -> if isExpired store now x
                    then updateFromBackend backend now
                    else pure $ Right $ getField @"value" $ getField @"value" x
    where
        updateFromBackend backend now = tryGetBackend backend namespace key >>= \case
            Left err -> pure $ Left err
            Right v  -> do
                atomicModifyIORef' (getField @"state" store) $ \stateRef -> (, ()) $ stateRef & lens `over`
                    (HM.insert key (Expirable v False now))
                pure $ Right $ getField @"value" v

getFlag :: Store -> Text -> StoreResult (Maybe Flag)
getFlag store key = getGeneric store "flags" key (field @"flags")

getSegment :: Store -> Text -> StoreResult (Maybe Segment)
getSegment store key = getGeneric store "segments" key (field @"segments")

upsertGeneric :: (ToJSON a) => Store -> Text -> Text -> Versioned (Maybe a)
    -> Lens' State (HashMap Text (Expirable (Versioned (Maybe a))))
    -> (Bool -> State -> State)
    -> StoreResult ()
upsertGeneric store namespace key versioned lens action = do
    case getField @"backend" store of
        Nothing      -> do
            void $ atomicModifyIORef' (getField @"state" store) $ \stateRef -> (, ()) $ upsertMemory stateRef
            pure $ Right ()
        Just backend -> do
            result <- (storeInterfaceUpsertFeature backend) namespace key (versionedToRaw versioned)
            case result of
                Left err      -> pure $ Left err
                Right updated -> if not updated then pure (Right ()) else do
                    now <- getMonotonicTime
                    void $ atomicModifyIORef' (getField @"state" store) $ \stateRef -> (, ()) $ stateRef
                        & lens `over` (HM.insert key (Expirable versioned False now))
                        & action True
                    pure $ Right ()
    where
        upsertMemory state = case HM.lookup key (state ^. lens) of
            Nothing       -> updateMemory state
            Just existing -> if (getField @"version" $ getField @"value" existing) < getField @"version" versioned
                then updateMemory state else state
        updateMemory state = state
            & lens `over` (HM.insert key (Expirable versioned False 0))
            & action False

upsertFlag :: Store -> Text -> Versioned (Maybe Flag) -> StoreResult ()
upsertFlag store key versioned = upsertGeneric store "flags" key versioned (field @"flags") postAction where
    postAction external state = if external
        then state & field @"allFlags" `over` (setField @"forceExpire" True)
        else state & (field @"allFlags" . field @"value") `over` updateAllFlags
    updateAllFlags allFlags = case getField @"value" versioned of
        Nothing   -> HM.delete key allFlags
        Just flag -> HM.insert key flag allFlags

upsertSegment :: Store -> Text -> Versioned (Maybe Segment) -> StoreResult ()
upsertSegment store key versioned = upsertGeneric store "segments" key versioned (field @"segments") postAction where
    postAction _ state = state

filterAndCacheFlags :: Store -> TimeSpec -> HashMap Text RawFeature -> IO (HashMap Text Flag)
filterAndCacheFlags store now raw = do
    let decoded  = HM.mapMaybe rawToVersioned raw
        allFlags = HM.mapMaybe (getField @"value") decoded
    atomicModifyIORef' (getField @"state" store) $ \state -> (, ()) $
        setField @"allFlags" (Expirable allFlags False now) $
            setField @"flags" (HM.map (\x -> Expirable x False now) decoded) state
    pure allFlags

getAllFlags :: Store -> StoreResult (HashMap Text Flag)
getAllFlags store = do
    state <- readIORef $ getField @"state" store
    let memoryFlags = pure $ Right $ getField @"value" $ getField @"allFlags" state
    case getField @"backend" store of
        Nothing      -> memoryFlags
        Just backend -> do
            now <- getMonotonicTime
            if not (isExpired store now $ getField @"allFlags" state)
                then memoryFlags
                else do
                    result <- (storeInterfaceAllFeatures backend) "flags"
                    case result of
                        Left err  -> pure (Left err)
                        Right raw -> do
                            filtered <- filterAndCacheFlags store now raw
                            pure (Right filtered)

isInitialized :: Store -> StoreResult Bool
isInitialized store = do
    state <- readIORef $ getField @"state" store
    let initialized = getField @"initialized" state
    if getField @"value" initialized
        then pure $ Right True
        else case getField @"backend" store of
            Nothing      -> pure $ Right False
            Just backend -> do
                now <- getMonotonicTime
                if isExpired store now initialized
                    then do
                        result <- storeInterfaceIsInitialized backend
                        case result of
                            Left err -> pure $ Left err
                            Right i  -> do
                                atomicModifyIORef' (getField @"state" store) $ \stateRef -> (, ()) $
                                    setField @"initialized" (Expirable i False now) stateRef
                                pure $ Right i
                    else pure $ Right False
