module LaunchDarkly.Server.DataSource.Internal
    ( DataSourceFactory
    , nullDataSourceFactory
    , DataSource(..)
    , DataSourceUpdates(..)
    , defaultDataSourceUpdates
    )
    where

import Data.IORef          (IORef, atomicModifyIORef')
import Data.HashMap.Strict (HashMap)
import Data.Text           (Text)
import GHC.Natural         (Natural)

import LaunchDarkly.Server.Config.ClientContext (ClientContext)
import LaunchDarkly.Server.Client.Status        (Status, transitionStatus)
import LaunchDarkly.Server.Features             (Segment, Flag)
import LaunchDarkly.Server.Store.Internal       (initializeStore, insertFlag, insertSegment, deleteFlag, deleteSegment, StoreHandle)

type DataSourceFactory = ClientContext -> DataSourceUpdates -> IO DataSource

nullDataSourceFactory :: DataSourceFactory
nullDataSourceFactory _ _ =
    pure $ DataSource (pure False) (pure ()) (pure ())

data DataSource = DataSource
    { dataSourceIsInitialized :: IO Bool
    , dataSourceStart :: IO ()
    , dataSourceStop :: IO ()
    }

data DataSourceUpdates = DataSourceUpdates
    { dataSourceUpdatesInit :: !(HashMap Text Flag -> HashMap Text Segment -> IO (Either Text ()))
    , dataSourceUpdatesInsertFlag :: !(Flag -> IO (Either Text ()))
    , dataSourceUpdatesInsertSegment :: !(Segment -> IO (Either Text ()))
    , dataSourceUpdatesDeleteFlag :: !(Text -> Natural -> IO (Either Text ()))
    , dataSourceUpdatesDeleteSegment :: !(Text -> Natural -> IO (Either Text ()))
    , dataSourceUpdatesSetStatus :: Status -> IO ()
    }

defaultDataSourceUpdates :: IORef Status -> StoreHandle IO -> DataSourceUpdates
defaultDataSourceUpdates status store =
    let modifyStatus status' = atomicModifyIORef' status (fmap (,()) (transitionStatus status')) 
    in DataSourceUpdates
        { dataSourceUpdatesInit = initializeStore store 
        , dataSourceUpdatesInsertFlag = insertFlag store 
        , dataSourceUpdatesInsertSegment = insertSegment store 
        , dataSourceUpdatesDeleteFlag = deleteFlag store 
        , dataSourceUpdatesDeleteSegment = deleteSegment store 
        , dataSourceUpdatesSetStatus = modifyStatus 
        }
