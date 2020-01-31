module Spec.Store (allTests) where

import           Test.HUnit
import           Data.Text                          (Text)
import           Data.HashMap.Strict                (HashMap)
import qualified Data.HashMap.Strict as             HM
import           GHC.Natural                        (Natural)

import           Util.Features                      (makeTestFlag)

import           LaunchDarkly.Server.Features       (Flag(..), VariationOrRollout(..))
import           LaunchDarkly.Server.Store.Internal

makeTestStore :: IO (StoreHandle IO)
makeTestStore = makeStoreIO Nothing 0

testInitializationEmpty :: Test
testInitializationEmpty = TestCase $ do
    store <- makeTestStore
    status1 <- getInitializedC store
    Right False @?= status1
    status2 <- storeHandleInitialize store HM.empty HM.empty
    Right () @?= status2
    status3 <- getInitializedC store
    Right True @?= status3

testInitializationWithFlags :: Test
testInitializationWithFlags = TestCase $ do
    store <- makeTestStore
    status1 <- getInitializedC store
    Right False @?= status1
    status2 <- storeHandleInitialize store flagsV HM.empty
    Right () @?= status2
    status3 <- getInitializedC store
    Right True @?= status3
    status4 <- storeHandleGetFlag store "a"
    Right (Just flagA) @?= status4
    status5 <- storeHandleAllFlags store
    Right flagsR @?= status5
    where
        flagA  = makeTestFlag "a" 52
        flagsR = HM.singleton "a" flagA
        flagsV = HM.singleton "a" (Versioned flagA 52)

testGetAndUpsertAndGetAndGetAll :: Test
testGetAndUpsertAndGetAndGetAll = TestCase $ do
    store <- makeTestStore
    status1 <- getFlagC store "a"
    Right Nothing @?= status1
    status2 <- upsertFlagC store "a" (Versioned (pure flag) 52)
    Right () @?= status2
    status3 <- getFlagC store "a"
    Right (pure flag) @=? status3
    status4 <- getAllFlagsC store
    (Right $ HM.singleton "a" flag) @?= status4
    where
        flag = makeTestFlag "a" 52

testUpsertRespectsVersion :: Test
testUpsertRespectsVersion = TestCase $ do
    store <- makeTestStore
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

allTests :: Test
allTests = TestList
    [ testInitializationEmpty
    , testInitializationWithFlags
    , testGetAndUpsertAndGetAndGetAll
    , testUpsertRespectsVersion
    ]
