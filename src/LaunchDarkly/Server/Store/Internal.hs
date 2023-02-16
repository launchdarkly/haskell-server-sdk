{-# LANGUAGE NamedFieldPuns #-}

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
    , PersistentDataStore (..)
    , SerializedItemDescriptor (..)
    , StoreHandle (..)
    , LaunchDarklyStoreRead (..)
    , LaunchDarklyStoreWrite (..)
    , ItemDescriptor (..)
    , makeStoreIO
    , insertFlag
    , deleteFlag
    , insertSegment
    , deleteSegment
    , initializeStore
    , createSerializedItemDescriptor
    , FeatureKey
    , FeatureNamespace
    , serializeWithPlaceholder
    , byteStringToVersionedData
    ) where

import Control.Lens (Lens', (%~), (^.))
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON (toJSON), decode, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Function ((&))
import Data.Generics.Product (HasField', field, getField, setField)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import System.Clock (Clock (Monotonic), TimeSpec, getTime)

import Data.Aeson.Types (Value (Bool))
import Data.Either.Extra (eitherToMaybe)
import LaunchDarkly.AesonCompat (KeyMap, deleteKey, emptyObject, insertKey, lookupKey, mapMaybeValues, mapValues, singleton)
import LaunchDarkly.Server.Features (Flag, Segment)

-- Store result not defined in terms of StoreResultM so we dont have to export.
type StoreResultM m a = m (Either Text a)

-- |
-- The result type for every `PersistentDataStore` function. Instead of throwing
-- an exception, any store related error should return `Left`. Exceptions
-- unrelated to the store should not be caught.
type StoreResult a = IO (Either Text a)

class LaunchDarklyStoreRead store m where
    getFlagC :: store -> Text -> StoreResultM m (Maybe Flag)
    getSegmentC :: store -> Text -> StoreResultM m (Maybe Segment)
    getAllFlagsC :: store -> StoreResultM m (KeyMap Flag)
    getInitializedC :: store -> StoreResultM m Bool

class LaunchDarklyStoreWrite store m where
    storeInitializeC :: store -> KeyMap (ItemDescriptor Flag) -> KeyMap (ItemDescriptor Segment) -> StoreResultM m ()
    upsertSegmentC :: store -> Text -> ItemDescriptor (Maybe Segment) -> StoreResultM m ()
    upsertFlagC :: store -> Text -> ItemDescriptor (Maybe Flag) -> StoreResultM m ()

data StoreHandle m = StoreHandle
    { storeHandleGetFlag :: !(Text -> StoreResultM m (Maybe Flag))
    , storeHandleGetSegment :: !(Text -> StoreResultM m (Maybe Segment))
    , storeHandleAllFlags :: !(StoreResultM m (KeyMap Flag))
    , storeHandleInitialized :: !(StoreResultM m Bool)
    , storeHandleInitialize :: !(KeyMap (ItemDescriptor Flag) -> KeyMap (ItemDescriptor Segment) -> StoreResultM m ())
    , storeHandleUpsertSegment :: !(Text -> ItemDescriptor (Maybe Segment) -> StoreResultM m ())
    , storeHandleUpsertFlag :: !(Text -> ItemDescriptor (Maybe Flag) -> StoreResultM m ())
    , storeHandleExpireAll :: !(StoreResultM m ())
    }
    deriving (Generic)

instance Monad m => LaunchDarklyStoreRead (StoreHandle m) m where
    getFlagC = storeHandleGetFlag
    getSegmentC = storeHandleGetSegment
    getAllFlagsC = storeHandleAllFlags
    getInitializedC = storeHandleInitialized

instance Monad m => LaunchDarklyStoreWrite (StoreHandle m) m where
    storeInitializeC = storeHandleInitialize
    upsertSegmentC = storeHandleUpsertSegment
    upsertFlagC = storeHandleUpsertFlag

initializeStore ::
    (LaunchDarklyStoreWrite store m, Monad m) =>
    store ->
    KeyMap Flag ->
    KeyMap Segment ->
    StoreResultM m ()
initializeStore store flags segments = storeInitializeC store (makeVersioned flags) (makeVersioned segments)
  where
    makeVersioned = mapValues (\f -> ItemDescriptor f (getField @"version" f))

insertFlag :: (LaunchDarklyStoreWrite store m, Monad m) => store -> Flag -> StoreResultM m ()
insertFlag store flag = upsertFlagC store (getField @"key" flag) $ ItemDescriptor (pure flag) (getField @"version" flag)

deleteFlag :: (LaunchDarklyStoreWrite store m, Monad m) => store -> Text -> Natural -> StoreResultM m ()
deleteFlag store key version = upsertFlagC store key $ ItemDescriptor Nothing version

insertSegment :: (LaunchDarklyStoreWrite store m, Monad m) => store -> Segment -> StoreResultM m ()
insertSegment store segment = upsertSegmentC store (getField @"key" segment) $ ItemDescriptor (pure segment) (getField @"version" segment)

deleteSegment :: (LaunchDarklyStoreWrite store m, Monad m) => store -> Text -> Natural -> StoreResultM m ()
deleteSegment store key version = upsertSegmentC store key $ ItemDescriptor Nothing version

makeStoreIO :: Maybe PersistentDataStore -> TimeSpec -> IO (StoreHandle IO)
makeStoreIO backend ttl = do
    state <-
        newIORef
            State
                { allFlags = Expirable emptyObject True 0
                , features = emptyObject
                , segments = emptyObject
                , initialized = Expirable False True 0
                }
    let store = Store state backend ttl
    pure
        StoreHandle
            { storeHandleGetFlag = getFlag store
            , storeHandleGetSegment = getSegment store
            , storeHandleAllFlags = getAllFlags store
            , storeHandleInitialized = isInitialized store
            , storeHandleInitialize = initialize store
            , storeHandleUpsertSegment = upsertSegment store
            , storeHandleUpsertFlag = upsertFlag store
            , storeHandleExpireAll = expireAllItems store >> (pure $ Right ())
            }

data Expirable a = Expirable
    { value :: !a
    , forceExpire :: !Bool
    , updatedOn :: !TimeSpec
    }
    deriving (Generic)

data ItemDescriptor a = ItemDescriptor
    { value :: !a
    , version :: !Natural
    }
    deriving (Generic)

-- The CacheableItem is used to store results from a persistent store.
--
-- The type is a Maybe because it is possible that a persistent store will not
-- have a record of a flag requested. We can store that result as a Nothing and
-- prevent subsequent evaluations from reaching across the network.
type CacheableItem a = Maybe (ItemDescriptor (Maybe a))

data State = State
    { allFlags :: !(Expirable (KeyMap Flag))
    , features :: !(KeyMap (Expirable (CacheableItem Flag)))
    , segments :: !(KeyMap (Expirable (CacheableItem Segment)))
    , initialized :: !(Expirable Bool)
    }
    deriving (Generic)

-- | Represents the key for a given feature.
type FeatureKey = Text

-- | Represents a namespace such as features or segments
type FeatureNamespace = Text

-- | The interface implemented by external stores for use by the SDK.
data PersistentDataStore = PersistentDataStore
    { persistentDataStoreAllFeatures :: !(FeatureNamespace -> StoreResult (KeyMap SerializedItemDescriptor))
    -- ^ A map of all features in a given namespace including deleted.
    , persistentDataStoreGetFeature :: !(FeatureNamespace -> FeatureKey -> StoreResult (Maybe SerializedItemDescriptor))
    -- ^ Return the value of a key in a namespace.
    , persistentDataStoreUpsertFeature :: !(FeatureNamespace -> FeatureKey -> SerializedItemDescriptor -> StoreResult Bool)
    -- ^ Upsert a given feature. Versions should be compared before upsert.
    -- The result should indicate if the feature was replaced or not.
    , persistentDataStoreIsInitialized :: !(StoreResult Bool)
    -- ^ Checks if the external store has been initialized, which may
    -- have been done by another instance of the SDK.
    , persistentDataStoreInitialize :: !(KeyMap (KeyMap SerializedItemDescriptor) -> StoreResult ())
    -- ^ A map of namespaces, and items in namespaces. The entire store state
    -- should be replaced with these values.
    }

-- | A record representing an object that can be persisted in an external store.
data SerializedItemDescriptor = SerializedItemDescriptor
    { item :: !(Maybe ByteString)
    -- ^ A serialized item. If the item is deleted or does not exist this
    -- should be `Nothing`.
    , version :: !Natural
    -- ^ The version of a given item. If the item does not exist this should
    -- be zero.
    , deleted :: !Bool
    -- ^ True if this is a placeholder (tombstone) for a deleted item.
    }
    deriving (Generic, Eq, Show)

-- |
-- Generate a 'ByteString' representation of the 'SerializedItemDescriptor'.
--
-- If the 'SerializedItemDescriptor' has either a 'Nothing' value, or is marked
-- as deleted, the ByteString representation will be a tombstone marker containing the version and deletion status.
--
-- Otherwise, the internal item representation is returned.
serializeWithPlaceholder :: SerializedItemDescriptor -> ByteString
serializeWithPlaceholder SerializedItemDescriptor {item = Nothing, version = version} = tombstonePlaceholder version
serializeWithPlaceholder SerializedItemDescriptor {deleted = True, version = version} = tombstonePlaceholder version
serializeWithPlaceholder SerializedItemDescriptor {item = Just item} = item

-- Generate the tombstone placeholder ByteString representation.
tombstonePlaceholder :: Natural -> ByteString
tombstonePlaceholder version = singleton "deleted" (Bool True) & insertKey "version" (toJSON version) & encode & toStrict

-- |
-- Partially decode the provided ByteString into a 'VersionedData' struct.
--
-- This is useful for persistent stores who need to perform version comparsions
-- before persisting data.
byteStringToVersionedData :: ByteString -> Maybe VersionedData
byteStringToVersionedData byteString = decode $ fromStrict byteString

data VersionedData = VersionedData
    { version :: Natural
    , deleted :: Bool
    }
    deriving (Generic, ToJSON, FromJSON)

data Store = Store
    { state :: !(IORef State)
    , backend :: !(Maybe PersistentDataStore)
    , timeToLive :: !TimeSpec
    }
    deriving (Generic)

expireAllItems :: Store -> IO ()
expireAllItems store = atomicModifyIORef' (getField @"state" store) $ \state ->
    (,()) $
        state
            & field @"allFlags" %~ expire
            & field @"initialized" %~ expire
            & field @"features" %~ mapValues expire
            & field @"segments" %~ mapValues expire
  where
    expire = setField @"forceExpire" True

isExpired :: Store -> TimeSpec -> Expirable a -> Bool
isExpired store now item =
    (isJust $ getField @"backend" store)
        && ( (getField @"forceExpire" item)
                || (getField @"timeToLive" store) + (getField @"updatedOn" item) < now
           )

getMonotonicTime :: IO TimeSpec
getMonotonicTime = getTime Monotonic

initialize :: Store -> KeyMap (ItemDescriptor Flag) -> KeyMap (ItemDescriptor Segment) -> StoreResult ()
initialize store flags segments = case getField @"backend" store of
    Nothing -> do
        atomicModifyIORef' (getField @"state" store) $ \state ->
            (,()) $
                state
                    & setField @"features" (mapValues (\f -> Expirable (Just f) True 0) $ c flags)
                    & setField @"segments" (mapValues (\f -> Expirable (Just f) True 0) $ c segments)
                    & setField @"allFlags" (Expirable (mapValues (getField @"value") flags) True 0)
                    & setField @"initialized" (Expirable True False 0)
        pure $ Right ()
    Just backend ->
        (persistentDataStoreInitialize backend) serializedItemMap >>= \case
            Left err -> pure $ Left err
            Right () -> expireAllItems store >> pure (Right ())
  where
    serializedItemMap =
        emptyObject
            & insertKey "features" (mapValues createSerializedItemDescriptor $ c flags)
            & insertKey "segments" (mapValues createSerializedItemDescriptor $ c segments)
    c x = mapValues (\f -> f & field @"value" %~ Just) x

serializedToItemDescriptor :: (FromJSON a, HasField' "version" a Natural) => SerializedItemDescriptor -> Either Text (ItemDescriptor (Maybe a))
serializedToItemDescriptor serializedItem = case getField @"item" serializedItem of
    Nothing -> pure $ ItemDescriptor Nothing (getField @"version" serializedItem)
    Just buffer -> do
        let versionedData = byteStringToVersionedData buffer
         in case versionedData of
                Nothing -> Left "failed decoding into VersionedData"
                Just VersionedData {deleted = True, version = version} -> pure $ ItemDescriptor Nothing version
                Just _ ->
                    let decodeResult = decode $ fromStrict buffer
                     in case decodeResult of
                            Nothing -> Left "failed decoding into ItemDescriptor"
                            Just decoded -> pure $ ItemDescriptor (Just decoded) (getField @"version" decoded)

createSerializedItemDescriptor :: (ToJSON a) => ItemDescriptor (Maybe a) -> SerializedItemDescriptor
createSerializedItemDescriptor ItemDescriptor {value = Nothing, version} = SerializedItemDescriptor {item = Nothing, version, deleted = True}
createSerializedItemDescriptor ItemDescriptor {value = Just item, version} = SerializedItemDescriptor {item = Just $ toStrict $ encode item, version, deleted = False}

tryGetBackend :: (FromJSON a, HasField' "version" a Natural) => PersistentDataStore -> Text -> Text -> StoreResult (Maybe (ItemDescriptor (Maybe a)))
tryGetBackend backend namespace key =
    ((persistentDataStoreGetFeature backend) namespace key) >>= \case
        Left err -> pure $ Left err
        Right Nothing -> pure $ Right Nothing
        Right (Just serializedItem) -> case serializedToItemDescriptor serializedItem of
            Left err -> pure $ Left err
            Right versioned -> pure $ Right $ Just versioned

getGeneric ::
    (FromJSON a, HasField' "version" a Natural) =>
    Store ->
    Text ->
    Text ->
    Lens' State (KeyMap (Expirable (CacheableItem a))) ->
    StoreResult (Maybe a)
getGeneric store namespace key lens = do
    state <- readIORef $ getField @"state" store
    case getField @"backend" store of
        Nothing -> case lookupKey key (state ^. lens) of
            Nothing -> pure $ Right Nothing
            Just cacheItem -> pure $ Right $ (getField @"value") =<< (getField @"value" cacheItem)
        Just backend -> do
            now <- getMonotonicTime
            case lookupKey key (state ^. lens) of
                Nothing -> updateFromBackend backend now
                Just cacheItem ->
                    if isExpired store now cacheItem
                        then updateFromBackend backend now
                        else pure $ Right $ (getField @"value") =<< (getField @"value" cacheItem)
  where
    updateFromBackend backend now =
        tryGetBackend backend namespace key >>= \case
            Left err -> pure $ Left err
            Right Nothing -> do
                atomicModifyIORef' (getField @"state" store) $ \stateRef ->
                    (,()) $
                        stateRef
                            & lens
                                %~ (insertKey key (Expirable Nothing False now))
                pure $ Right Nothing
            Right (Just v) -> do
                atomicModifyIORef' (getField @"state" store) $ \stateRef ->
                    (,()) $
                        stateRef
                            & lens
                                %~ (insertKey key (Expirable (Just v) False now))
                pure $ Right $ getField @"value" v

getFlag :: Store -> Text -> StoreResult (Maybe Flag)
getFlag store key = getGeneric store "features" key (field @"features")

getSegment :: Store -> Text -> StoreResult (Maybe Segment)
getSegment store key = getGeneric store "segments" key (field @"segments")

upsertGeneric ::
    (ToJSON a) =>
    Store ->
    Text ->
    Text ->
    ItemDescriptor (Maybe a) ->
    Lens' State (KeyMap (Expirable (CacheableItem a))) ->
    (Bool -> State -> State) ->
    StoreResult ()
upsertGeneric store namespace key versioned lens action = do
    case getField @"backend" store of
        Nothing -> do
            void $ atomicModifyIORef' (getField @"state" store) $ \stateRef -> (,()) $ upsertMemory stateRef
            pure $ Right ()
        Just backend -> do
            result <- (persistentDataStoreUpsertFeature backend) namespace key (createSerializedItemDescriptor versioned)
            case result of
                Left err -> pure $ Left err
                Right updated ->
                    if not updated
                        then pure (Right ())
                        else do
                            now <- getMonotonicTime
                            void $ atomicModifyIORef' (getField @"state" store) $ \stateRef ->
                                (,()) $
                                    stateRef
                                        & lens %~ (insertKey key (Expirable (Just versioned) False now))
                                        & action True
                            pure $ Right ()
  where
    upsertMemory state = case lookupKey key (state ^. lens) of
        Nothing -> updateMemory state
        Just cacheItem -> case getField @"value" cacheItem of
            Nothing -> updateMemory state
            Just existing ->
                if (getField @"version" existing) < getField @"version" versioned
                    then updateMemory state
                    else state
    updateMemory state =
        state
            & lens %~ (insertKey key (Expirable (Just versioned) False 0))
            & action False

upsertFlag :: Store -> Text -> ItemDescriptor (Maybe Flag) -> StoreResult ()
upsertFlag store key versioned = upsertGeneric store "features" key versioned (field @"features") postAction
  where
    postAction external state =
        if external
            then state & field @"allFlags" %~ (setField @"forceExpire" True)
            else state & (field @"allFlags" . field @"value") %~ updateAllFlags
    updateAllFlags allFlags = case getField @"value" versioned of
        Nothing -> deleteKey key allFlags
        Just flag -> insertKey key flag allFlags

upsertSegment :: Store -> Text -> ItemDescriptor (Maybe Segment) -> StoreResult ()
upsertSegment store key versioned = upsertGeneric store "segments" key versioned (field @"segments") postAction
  where
    postAction _ state = state

filterAndCacheFlags :: Store -> TimeSpec -> KeyMap SerializedItemDescriptor -> IO (KeyMap Flag)
filterAndCacheFlags store now serializedMap = do
    let decoded = mapMaybeValues (eitherToMaybe . serializedToItemDescriptor) serializedMap
        allFlags = mapMaybeValues (getField @"value") decoded
    atomicModifyIORef' (getField @"state" store) $ \state ->
        (,()) $
            setField @"allFlags" (Expirable allFlags False now) $
                setField @"features" (mapValues (\x -> Expirable (Just x) False now) decoded) state
    pure allFlags

getAllFlags :: Store -> StoreResult (KeyMap Flag)
getAllFlags store = do
    state <- readIORef $ getField @"state" store
    let memoryFlags = pure $ Right $ getField @"value" $ getField @"allFlags" state
    case getField @"backend" store of
        Nothing -> memoryFlags
        Just backend -> do
            now <- getMonotonicTime
            if not (isExpired store now $ getField @"allFlags" state)
                then memoryFlags
                else do
                    result <- (persistentDataStoreAllFeatures backend) "features"
                    case result of
                        Left err -> pure (Left err)
                        Right serializedMap -> do
                            filtered <- filterAndCacheFlags store now serializedMap
                            pure (Right filtered)

isInitialized :: Store -> StoreResult Bool
isInitialized store = do
    state <- readIORef $ getField @"state" store
    let initialized = getField @"initialized" state
    if getField @"value" initialized
        then pure $ Right True
        else case getField @"backend" store of
            Nothing -> pure $ Right False
            Just backend -> do
                now <- getMonotonicTime
                if isExpired store now initialized
                    then do
                        result <- persistentDataStoreIsInitialized backend
                        case result of
                            Left err -> pure $ Left err
                            Right i -> do
                                atomicModifyIORef' (getField @"state" store) $ \stateRef ->
                                    (,()) $
                                        setField @"initialized" (Expirable i False now) stateRef
                                pure $ Right i
                    else pure $ Right False
