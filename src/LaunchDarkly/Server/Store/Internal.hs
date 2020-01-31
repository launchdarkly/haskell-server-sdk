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
    , expireAllItems
    , insertFlag
    , deleteFlag
    , insertSegment
    , deleteSegment
    , initializeStore
    ) where

import           Control.Monad                (void)
import           Control.Lens                 (Lens', (%~), (^.))
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

type StoreResultM m a = m (Either Text a)

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
    { storeHandleGetFlag       :: Text -> StoreResultM m (Maybe Flag)
    , storeHandleGetSegment    :: Text -> StoreResultM m (Maybe Segment)
    , storeHandleAllFlags      :: StoreResultM m (HashMap Text Flag)
    , storeHandleInitialized   :: StoreResultM m Bool
    , storeHandleInitialize    :: HashMap Text (Versioned Flag) -> HashMap Text (Versioned Segment) -> StoreResultM m ()
    , storeHandleUpsertSegment :: Text -> Versioned (Maybe Segment) -> StoreResultM m ()
    , storeHandleUpsertFlag    :: Text -> Versioned (Maybe Flag) -> StoreResultM m ()
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
        { storeHandleGetFlag       = getFlag       store
        , storeHandleGetSegment    = getSegment    store
        , storeHandleAllFlags      = getAllFlags   store
        , storeHandleInitialized   = isInitialized store
        , storeHandleInitialize    = initialize    store
        , storeHandleUpsertSegment = upsertSegment store
        , storeHandleUpsertFlag    = upsertFlag    store
        }

data Expirable a = Expirable
    { value       :: a
    , forceExpire :: Bool
    , updatedOn   :: TimeSpec
    } deriving (Generic)

data Versioned a = Versioned
    { value   :: a
    , version :: Natural
    } deriving (Generic)

data State = State
    { allFlags    :: Expirable (HashMap Text Flag)
    , flags       :: HashMap Text (Expirable (Versioned (Maybe Flag)))
    , segments    :: HashMap Text (Expirable (Versioned (Maybe Segment)))
    , initialized :: Expirable Bool
    } deriving (Generic)

type StoreResult a = StoreResultM IO a

data StoreInterface = StoreInterace
    { storeInterfaceAllFeatures   :: Text -> StoreResult (HashMap Text RawFeature)
    , storeInterfaceGetFeature    :: Text -> Text -> StoreResult RawFeature
    , storeInterfaceUpsertFeature :: Text -> Text -> RawFeature -> StoreResult Bool
    , storeInterfaceIsInitialized :: StoreResult Bool
    , storeInterfaceInitialize    :: HashMap Text (HashMap Text RawFeature) -> StoreResult ()
    } deriving (Generic)

data RawFeature = RawFeature
    { buffer  :: Maybe ByteString
    , version :: Natural
    } deriving (Generic)

data Store = Store
    { state      :: IORef State
    , backend    :: Maybe StoreInterface
    , timeToLive :: TimeSpec
    } deriving (Generic)

expireAllItems :: Store -> IO ()
expireAllItems store = atomicModifyIORef' (getField @"state" store) $ \state -> (flip (,) ()) $ state
    & field @"allFlags"    %~ expire
    & field @"initialized" %~ expire
    & field @"flags"       %~ HM.map expire
    & field @"segments"    %~ HM.map expire
    where expire = setField @"forceExpire" True

isExpired :: Store -> TimeSpec -> Expirable a -> Bool
isExpired store now item = (isJust $ getField @"backend" store) && ((getField @"forceExpire" item)
    || (getField @"timeToLive" store) + (getField @"updatedOn" item) < now)

getMonotonicTime :: IO TimeSpec
getMonotonicTime = getTime Monotonic

initialize :: Store -> HashMap Text (Versioned Flag) -> HashMap Text (Versioned Segment) -> StoreResult ()
initialize store flags segments = case getField @"backend" store of
    Nothing      -> do
        atomicModifyIORef' (getField @"state" store) $ \state -> (flip (,) ()) $ state
            & setField @"flags"       (HM.map (\f -> Expirable f True 0) $ c flags)
            & setField @"segments"    (HM.map (\f -> Expirable f True 0) $ c segments)
            & setField @"allFlags"    (Expirable (HM.map (\f -> getField @"value" f) flags) True 0)
            & setField @"initialized" (Expirable True False 0)
        pure $ Right ()
    Just backend -> (getField @"storeInterfaceInitialize" backend) raw
    where
        raw = HM.empty
            & HM.insert "flags"    (HM.map versionedToRaw $ c flags)
            & HM.insert "segments" (HM.map versionedToRaw $ c segments)
        c x = HM.map (\f -> f & field @"value" %~ Just) x

rawToVersioned :: (FromJSON a) => RawFeature -> Maybe (Versioned (Maybe a))
rawToVersioned raw = case getField @"buffer" raw of
    Nothing     -> pure $ Versioned Nothing (getField @"version" raw)
    Just buffer -> case decode $ fromStrict buffer of
        Nothing      -> Nothing
        Just decoded -> pure $ Versioned decoded (getField @"version" raw)

versionedToRaw :: (ToJSON a) => Versioned (Maybe a) -> RawFeature
versionedToRaw versioned = case getField @"value" versioned of
    Nothing -> RawFeature Nothing $ getField @"version" versioned
    Just x  -> RawFeature (pure $ toStrict $ encode x) $ getField @"version" versioned

tryGetBackend :: (FromJSON a) => StoreInterface -> Text -> Text -> StoreResult (Versioned (Maybe a))
tryGetBackend backend namespace key =
    ((getField @"storeInterfaceGetFeature" backend) namespace key) >>= \case
        Left err  -> pure $ Left err
        Right raw -> case rawToVersioned raw of
            Nothing        -> pure $ Left "failed to decode from external store"
            Just versioned -> pure $ Right versioned

getGeneric :: FromJSON a => Store -> Text -> Text
    -> (Lens' State (HashMap Text (Expirable (Versioned (Maybe a)))))
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
                atomicModifyIORef' (getField @"state" store) $ \stateRef -> (flip (,) ()) $ stateRef & lens %~
                    (HM.insert key (Expirable v False now))
                pure $ Right $ getField @"value" v

getFlag :: Store -> Text -> StoreResult (Maybe Flag)
getFlag store key = getGeneric store "flags" key (field @"flags")

getSegment :: Store -> Text -> StoreResult (Maybe Segment)
getSegment store key = getGeneric store "segments" key (field @"segments")

upsertGeneric :: (ToJSON a) => Store -> Text -> Text -> Versioned (Maybe a)
    -> (Lens' State (HashMap Text (Expirable (Versioned (Maybe a)))))
    -> (Bool -> State -> State)
    -> StoreResult ()
upsertGeneric store namespace key versioned lens action = do
    case getField @"backend" store of
        Nothing      -> do
            void $ atomicModifyIORef' (getField @"state" store) $ \stateRef -> (flip (,) ()) $ upsertMemory stateRef
            pure $ Right ()
        Just backend -> do
            result <- (getField @"storeInterfaceUpsertFeature" backend) namespace key (versionedToRaw versioned)
            case result of
                Left err      -> pure $ Left err
                Right updated -> if not updated then pure (Right ()) else do
                    void $ atomicModifyIORef' (getField @"state" store) $ \stateRef -> (flip (,) ())
                        (action True stateRef)
                    pure $ Right ()
    where
        upsertMemory state = case HM.lookup key (state ^. lens) of
            Nothing       -> updateMemory state
            Just existing -> if (getField @"version" $ getField @"value" existing) < getField @"version" versioned
                then updateMemory state else state
        updateMemory state = state
            & lens %~ (HM.insert key (Expirable versioned False 0))
            & action False

upsertFlag :: Store -> Text -> Versioned (Maybe Flag) -> StoreResult ()
upsertFlag store key versioned = upsertGeneric store "flags" key versioned (field @"flags") postAction where
    postAction external state = if external
        then state & field @"allFlags" %~ (setField @"forceExpire" True)
        else state & (field @"allFlags" . field @"value") %~ updateAllFlags
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
    atomicModifyIORef' (getField @"state" store) $ \state -> (flip (,) ()) $
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
                    result <- (getField @"storeInterfaceAllFeatures" backend) "flags"
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
                        result <- getField @"storeInterfaceIsInitialized" backend
                        case result of
                            Left err -> pure $ Left err
                            Right i  -> do
                                atomicModifyIORef' (getField @"state" store) $ \stateRef -> (flip (,) ()) $
                                    setField @"initialized" (Expirable i False now) stateRef
                                pure $ Right i
                    else pure $ Right False
