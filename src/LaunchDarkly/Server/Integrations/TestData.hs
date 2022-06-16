-- |
-- A mechanism for providing dynamically updatable feature flag state in a simplified form to an SDK
-- client in test scenarios.
--
-- Unlike "LaunchDarkly.Server.Integrations.FileData", this mechanism does not use any external resources. It provides only
-- the data that the application has put into it using the 'update' function.
--
-- @
-- td <- TestData.newTestData
-- update td =<< (flag td "flag-key-1"
--                 \<&\> booleanFlag
--                 \<&\> variationForAllUsers True)
--
-- let config = makeConfig "sdkKey"
--                 & configSetDataSourceFactory (dataSourceFactory td)
-- client <- makeClient config
--
-- -- flags can be updated at any time:
-- update td =<<
--    (flag td "flag-key-2"
--          \<&\> variationForUser "some-user-key" True
--          \<&\> fallthroughVariation False)
-- @
--
-- The above example uses a simple boolean flag, but more complex configurations are possible using
-- the methods of the 'FlagBuilder' that is returned by 'flag'. 'FlagBuilder'
-- supports many of the ways a flag can be configured on the LaunchDarkly dashboard, but does not
-- currently support:
--
--      1. Rule operators other than "in" and "not in"
--      2. Percentage rollouts.
--
-- If the same 'TestData' instance is used to configure multiple 'LaunchDarkly.Server.Client.Client' instances,
-- any changes made to the data will propagate to all of the @Client@s.
--
-- see "LaunchDarkly.Server.Integrations.FileData"
--
-- @since 2.2.1
module LaunchDarkly.Server.Integrations.TestData
    ( TestData
    , newTestData
    , flag
    , update
    , dataSourceFactory

    -- * FlagBuilder
    , FlagBuilder
    , booleanFlag
    , on
    , fallthroughVariation
    , offVariation
    , variationForAllUsers
    , valueForAllUsers
    , variationForUser
    , variations
    , ifMatch
    , ifNotMatch
    , VariationIndex

    -- * FlagRuleBuilder
    , FlagRuleBuilder
    , andMatch
    , andNotMatch
    , thenReturn
    )
    where

import           Control.Concurrent.MVar                               (MVar, modifyMVar_, newMVar, newEmptyMVar, readMVar, putMVar)
import           Control.Monad                                         (void)
import           Data.Foldable                                         (traverse_)
import           Data.HashMap.Strict                                   (HashMap)
import qualified Data.HashMap.Strict                                   as HM
import           Data.IntMap.Strict                                    (IntMap)
import qualified Data.IntMap.Strict                                    as IntMap
import           Data.Map.Strict                                       (Map)
import qualified Data.Map.Strict                                       as Map
import qualified Data.Maybe                                            as Maybe
import           Data.Text                                             (Text)

import           Data.Generics.Product                                 (getField)
import           LaunchDarkly.Server.DataSource.Internal
import qualified LaunchDarkly.Server.Features                          as Features
import           LaunchDarkly.Server.Integrations.TestData.FlagBuilder

dataSourceFactory :: TestData -> DataSourceFactory
dataSourceFactory (TestData ref) _clientContext dataSourceUpdates = do
    listenerIdRef <- newEmptyMVar
    let upsert flag = void $ dataSourceUpdatesInsertFlag dataSourceUpdates flag
        dataSourceStart = do
            modifyMVar_ ref $ \td -> do
                void $ dataSourceUpdatesInit dataSourceUpdates (currentFlags td) mempty
                let (td', listenerId) = addDataSourceListener td upsert
                putMVar listenerIdRef listenerId
                pure td'
        dataSourceIsInitialized =
            pure True
        dataSourceStop =
            modifyMVar_ ref $ \td ->
                removeDataSourceListener td <$> readMVar listenerIdRef
    pure $ DataSource {..}

newtype TestData = TestData (MVar TestData')

type TestDataListener = Features.Flag -> IO ()

data TestData' = TestData'
    { flagBuilders             :: Map Text FlagBuilder
    , currentFlags             :: HashMap Text Features.Flag
    , nextDataSourceListenerId :: Int
    , dataSourceListeners      :: IntMap TestDataListener
    }

-- | Creates a new instance of the test data source.
newTestData :: IO TestData -- ^ a new configurable test data source
newTestData =
    TestData <$> newMVar (TestData' mempty mempty 0 mempty)

addDataSourceListener :: TestData' -> TestDataListener -> (TestData', Int)
addDataSourceListener td listener =
    ( td{ nextDataSourceListenerId = nextDataSourceListenerId td + 1
        , dataSourceListeners = IntMap.insert (nextDataSourceListenerId td) listener (dataSourceListeners td)
        }
    , nextDataSourceListenerId td
    )

removeDataSourceListener :: TestData' -> Int -> TestData'
removeDataSourceListener td listenerId =
    td{ dataSourceListeners =
            IntMap.delete listenerId (dataSourceListeners td)
      }

-- |
--  Creates or copies a 'FlagBuilder' for building a test flag configuration.
--
--  If this flag key has already been defined in this 'TestData' instance, then the builder
--  starts with the same configuration that was last provided for this flag.
--
--  Otherwise, it starts with a new default configuration in which the flag has @True@ and
--  @False@ variations, is @True@ for all users when targeting is turned on and
--  @False@ otherwise, and currently has targeting turned on. You can change any of those
--  properties, and provide more complex behavior, using the 'FlagBuilder' methods.
--
--  Once you have set the desired configuration, pass the builder to 'update'.
--
--  see 'update'
flag :: TestData
     -> Text  -- ^ the flag key
     -> IO FlagBuilder -- ^ a flag configuration builder
flag (TestData ref) key = do
    td <- readMVar ref
    pure $ Maybe.fromMaybe (booleanFlag $ newFlagBuilder key)
         $ Map.lookup key (flagBuilders td)

-- |
--  Updates the test data with the specified flag configuration.
--
--  This has the same effect as if a flag were added or modified on the LaunchDarkly dashboard.
--  It immediately propagates the flag change to any 'LaunchDarkly.Server.Client.Client' instance(s) that you have
--  already configured to use this 'TestData'. If no @Client@ has been started yet,
--  it simply adds this flag to the test data which will be provided to any @Client@ that
--  you subsequently configure.
--
--  Any subsequent changes to this 'FlagBuilder' instance do not affect the test data,
--  unless you call 'update'
--
--  see 'flag'
update :: TestData
       -> FlagBuilder -- ^ a flag configuration builder
       -> IO ()
update (TestData ref) fb =
    modifyMVar_ ref $ \td -> do
        let key = fbKey fb
            mOldFlag = HM.lookup key (currentFlags td)
            oldFlagVersion = maybe 0 (getField @"version") mOldFlag
            newFlag = buildFlag (oldFlagVersion + 1) fb
            td' = td{ flagBuilders = Map.insert key fb (flagBuilders td)
                    , currentFlags = HM.insert key newFlag (currentFlags td)
                    }
        notifyListeners td newFlag
        pure td'
 where
     notifyListeners td newFlag =
        traverse_ ($ newFlag) (dataSourceListeners td)
