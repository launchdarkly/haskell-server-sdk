module Spec.Redis (allTests) where

import           Test.HUnit
import           Data.Text                                (Text)
import qualified Database.Redis as                        R
import           Data.HashMap.Strict                      (HashMap)
import qualified Data.HashMap.Strict as                   HM
import           Control.Monad                            (void)
import           GHC.Natural                              (Natural)
import           GHC.Int                                  (Int64)
import           System.Clock                             (TimeSpec(..))

import           Util.Features                            (makeTestFlag)

import           Spec.Store                               (testWithStore)

import           LaunchDarkly.Server.Features             (Flag(..), VariationOrRollout(..))
import           LaunchDarkly.Server.Store.Internal
import           LaunchDarkly.Server.Store.Redis.Internal

freshConnection :: IO R.Connection
freshConnection = do
    con <- R.checkedConnect R.defaultConnectInfo
    void $ R.runRedis con $ R.flushall
    pure con

testUpsertRace :: Test
testUpsertRace = TestCase $ do
    con <- freshConnection
    let config = makeRedisStoreConfig con
    backend <- makeRedisStore config
    store <- makeStoreIO (pure backend) $ TimeSpec 0 0
    let hook = (insertFlag store $ makeTestFlag "a" 2) >>= (Right () @=?)
    redisUpsertInternal hook config "flags" "a" (versionedToRaw $ Versioned (pure $ makeTestFlag "a" 1) 1)
        >>= (Right False @=?)
    getFlagC store "a" >>= (Right (pure $ makeTestFlag "a" 2) @=?)

prepareRedisStore :: Int64 -> IO (StoreHandle IO)
prepareRedisStore ttl = do
    con <- freshConnection
    backend <- makeRedisStore $ makeRedisStoreConfig con
    makeStoreIO (pure backend) $ TimeSpec ttl 0

allTests :: Test
allTests = TestList
    [ testWithStore $ prepareRedisStore 10
    , testWithStore $ prepareRedisStore 0
    , testUpsertRace
    ]
