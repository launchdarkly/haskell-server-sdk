module Spec.Store (allTests, testWithStore) where

import           Test.HUnit
import           Data.Text                          (Text)
import           Data.HashMap.Strict                (HashMap)
import qualified Data.HashMap.Strict as             HM
import           Control.Monad                      (void)
import           GHC.Natural                        (Natural)
import           GHC.Int                            (Int64)
import           System.Clock                       (TimeSpec(..))

import           Util.Features                      (makeTestFlag)

import           LaunchDarkly.Server.Features       (Flag(..), VariationOrRollout(..))
import           LaunchDarkly.Server.Store.Internal
import           LaunchDarkly.Server.Store.Redis

testInitializationEmpty ::  IO (StoreHandle IO) -> Test
testInitializationEmpty makeStore = TestCase $ do
    store <- makeStore
    status1 <- getInitializedC store
    Right False @=? status1
    status2 <- storeHandleInitialize store HM.empty HM.empty
    Right () @=? status2
    status3 <- getInitializedC store
    Right True @=? status3

testInitializationWithFlags :: IO (StoreHandle IO) -> Test
testInitializationWithFlags makeStore = TestCase $ do
    store <- makeStore
    status1 <- getInitializedC store
    Right False @=? status1
    status2 <- storeHandleInitialize store flagsV HM.empty
    Right () @=? status2
    status3 <- getInitializedC store
    Right True @=? status3
    status4 <- storeHandleGetFlag store "a"
    Right (Just flagA) @=? status4
    status5 <- storeHandleAllFlags store
    Right flagsR @=? status5
    where
        flagA  = makeTestFlag "a" 52
        flagsR = HM.singleton "a" flagA
        flagsV = HM.singleton "a" (Versioned flagA 52)

testGetAndUpsertAndGetAndGetAll :: IO (StoreHandle IO) -> Test
testGetAndUpsertAndGetAndGetAll makeStore = TestCase $ do
    store <- makeStore
    status1 <- getFlagC store "a"
    Right Nothing @=? status1
    status2 <- upsertFlagC store "a" (Versioned (pure flag) 52)
    Right () @=? status2
    status3 <- getFlagC store "a"
    Right (pure flag) @=? status3
    status4 <- getAllFlagsC store
    (Right $ HM.singleton "a" flag) @=? status4
    where
        flag = makeTestFlag "a" 52

testUpsertRespectsVersion :: IO (StoreHandle IO) -> Test
testUpsertRespectsVersion makeStore = TestCase $ do
    store <- makeStore
    status1 <- upsertFlagC store "a" (Versioned (pure $ makeTestFlag "a" 1) 1)
    Right () @=? status1
    status2 <- upsertFlagC store "a" (Versioned (pure $ makeTestFlag "a" 2) 2)
    Right () @=? status2
    status3 <- getFlagC store "a"
    Right (pure $ makeTestFlag "a" 2) @=? status3
    status4 <- getAllFlagsC store
    Right (HM.singleton "a" $ makeTestFlag "a" 2) @=? status4
    status5 <- upsertFlagC store "a" (Versioned (pure $ makeTestFlag "a" 1) 1)
    Right () @=? status5
    status6 <- getFlagC store "a"
    Right (pure $ makeTestFlag "a" 2) @=? status6
    status7 <- getAllFlagsC store
    Right (HM.singleton "a" $ makeTestFlag "a" 2) @=? status7
    status8 <- upsertFlagC store "a" (Versioned Nothing 3)
    Right () @=? status8
    status9 <- getFlagC store "a"
    Right Nothing @=? status9
    status10 <- getAllFlagsC store
    Right HM.empty @=? status10

testWithStore :: IO (StoreHandle IO) -> Test
testWithStore makeStore = TestList $ map (\f -> f makeStore)
    [ testInitializationEmpty
    , testInitializationWithFlags
    , testGetAndUpsertAndGetAndGetAll
    , testUpsertRespectsVersion
    ]

allTests :: Test
allTests = TestList
    [ testWithStore $ makeStoreIO Nothing 0
    ]
