module LaunchDarkly.Server.Store.Memory (makeMemoryStoreIO) where

import           Data.Text                 (Text)
import qualified Data.HashMap.Strict   as  HM
import           Data.Generics.Product     (HasField', getField)
import           Data.IORef                (atomicModifyIORef, writeIORef, readIORef, newIORef)
import           Control.Monad             (void)
import           Control.Arrow             (first, second)
import           GHC.Natural               (Natural)

import           LaunchDarkly.Server.Store (StoreHandle(..), PotentialItem(..))

redactDeleted :: HasField' "deleted" r Bool => (PotentialItem r) -> Maybe r
redactDeleted (ExistingItem x)  = if getField @"deleted" x then Nothing else Just x
redactDeleted _                 = Nothing

fromJustRight :: HasField' "deleted" r Bool => Maybe (PotentialItem r) -> Maybe r
fromJustRight Nothing                  = Nothing
fromJustRight (Just (DeletedItem _ _)) = Nothing
fromJustRight (Just (ExistingItem x))  = if getField @"deleted" x then Nothing else Just x

getKey :: HasField' "key" r Text => PotentialItem r -> Text
getKey (DeletedItem key version) = key
getKey (ExistingItem item)       = getField @"key" item

getVersion :: HasField' "version" r Natural => PotentialItem r -> Natural
getVersion (DeletedItem key version) = version
getVersion (ExistingItem item)       = getField @"version" item

unitPair :: x -> (x, ())
unitPair x = (x, ())

upsertHelper store side key value = void $ atomicModifyIORef store $ \store -> unitPair $ flip side store $ \items ->
    case HM.lookup key items of
        Nothing      -> HM.insert key value items
        Just current -> if getVersion value > getVersion current
            then HM.insert key value items else items

makeMemoryStoreIO :: IO (StoreHandle IO)
makeMemoryStoreIO = newIORef (HM.empty, HM.empty) >>= \store -> pure StoreHandle
    { getFlagHandle         = \key -> readIORef store >>= pure . fromJustRight . HM.lookup key . fst
    , getSegmentHandle      = \key -> readIORef store >>= pure . fromJustRight . HM.lookup key . snd
    , getAllFlagsHandle     = readIORef store >>= pure . HM.mapMaybe redactDeleted . fst
    , initializeStoreHandle = \flags segments -> writeIORef store (HM.map ExistingItem flags, HM.map ExistingItem segments)
    , upsertFlagHandle      = \value -> upsertHelper store first (getKey value) value
    , upsertSegmentHandle   = \value -> upsertHelper store second (getKey value) value
    }
