module LaunchDarkly.Server.Store
    ( deleteFlag
    , insertFlag
    , deleteSegment
    , insertSegment
    , StoreHandle(..)
    , LaunchDarklyStoreRead(..)
    , LaunchDarklyStoreWrite(..)
    , LaunchDarklyStore(..)
    , PotentialItem(..)
    ) where

import Data.Text                    (Text)
import Data.HashMap.Strict          (HashMap)
import GHC.Natural                  (Natural)
import GHC.Generics                 (Generic)

import LaunchDarkly.Server.Features (Segment, Flag)

data PotentialItem a =
      DeletedItem { key :: Text, version :: Natural }
    | ExistingItem a
    deriving (Generic)

class LaunchDarklyStoreRead store m where
  getFlag     :: store -> Text -> m (Maybe Flag)
  getSegment  :: store -> Text -> m (Maybe Segment)
  getAllFlags :: store -> m (HashMap Text Flag)

class LaunchDarklyStoreWrite store m where
  storeInitialize :: store -> HashMap Text Flag -> HashMap Text Segment -> m ()
  upsertSegment   :: store -> PotentialItem Segment -> m ()
  upsertFlag      :: store -> PotentialItem Flag -> m ()

class (LaunchDarklyStoreRead s m, LaunchDarklyStoreWrite s m) => LaunchDarklyStore s m

data StoreHandle m = StoreHandle
    { getFlagHandle         :: Text -> m (Maybe Flag)
    , getSegmentHandle      :: Text -> m (Maybe Segment)
    , getAllFlagsHandle     :: m (HashMap Text Flag)
    , initializeStoreHandle :: HashMap Text Flag -> HashMap Text Segment -> m ()
    , upsertSegmentHandle   :: PotentialItem Segment -> m ()
    , upsertFlagHandle      :: PotentialItem Flag -> m ()
    } deriving (Generic)

instance Monad m => LaunchDarklyStoreRead (StoreHandle m) m where
  getFlag     = getFlagHandle
  getSegment  = getSegmentHandle
  getAllFlags = getAllFlagsHandle

instance Monad m => LaunchDarklyStoreWrite (StoreHandle m) m where
  storeInitialize = initializeStoreHandle
  upsertFlag      = upsertFlagHandle
  upsertSegment   = upsertSegmentHandle

insertFlag :: (LaunchDarklyStoreWrite store m, Monad m) => store -> Flag -> m ()
insertFlag store flag = upsertFlag store $ ExistingItem flag

deleteFlag :: (LaunchDarklyStoreWrite store m, Monad m) => store -> Text -> Natural -> m ()
deleteFlag store key version = upsertFlag store $ DeletedItem key version

insertSegment :: (LaunchDarklyStoreWrite store m, Monad m) => store -> Segment -> m ()
insertSegment store flag = upsertSegment store $ ExistingItem flag

deleteSegment :: (LaunchDarklyStoreWrite store m, Monad m) => store -> Text -> Natural -> m ()
deleteSegment store key version = upsertSegment store $ DeletedItem key version
