module Spec.Store (allTests, testWithStore) where

import Control.Monad (void)
import Data.Text (Text)
import GHC.Int (Int64)
import GHC.Natural (Natural)
import System.Clock (TimeSpec (..))
import Test.HUnit

import Util.Features (makeTestFlag, makeTestSegment)

import LaunchDarkly.AesonCompat (emptyObject, singleton)
import LaunchDarkly.Server.Features (Flag (..), VariationOrRollout (..))
import LaunchDarkly.Server.Store.Internal
import LaunchDarkly.Server.Store.Redis

testInitializationEmpty :: IO (StoreHandle IO) -> Test
testInitializationEmpty makeStore = TestCase $ do
    store <- makeStore
    getInitializedC store >>= (pure False @=?)
    storeHandleInitialize store emptyObject emptyObject >>= (pure () @=?)
    getInitializedC store >>= (pure True @=?)

testInitializationWithFeatures :: IO (StoreHandle IO) -> Test
testInitializationWithFeatures makeStore = TestCase $ do
    store <- makeStore
    getInitializedC store >>= (pure False @=?)
    storeHandleInitialize store flagsV segmentsV >>= (pure () @=?)
    getInitializedC store >>= (pure True @=?)
    storeHandleGetFlag store "a" >>= (pure (pure flagA) @=?)
    storeHandleAllFlags store >>= (pure flagsR @=?)
    storeHandleGetSegment store "a" >>= (pure (pure segmentA) @=?)
  where
    segmentA = makeTestSegment "a" 50
    flagA = makeTestFlag "a" 52
    flagsR = singleton "a" flagA
    flagsV = singleton "a" (Versioned flagA 52)
    segmentsV = singleton "a" (Versioned segmentA 50)

testGetAndUpsertAndGetAndGetAllFlags :: IO (StoreHandle IO) -> Test
testGetAndUpsertAndGetAndGetAllFlags makeStore = TestCase $ do
    store <- makeStore
    getFlagC store "a" >>= (pure Nothing @=?)
    upsertFlagC store "a" (Versioned (pure flag) 52) >>= (pure () @=?)
    getFlagC store "a" >>= (pure (pure flag) @=?)
    getAllFlagsC store >>= (pure (singleton "a" flag) @=?)
  where
    flag = makeTestFlag "a" 52

testGetAndUpsertAndGetSegment :: IO (StoreHandle IO) -> Test
testGetAndUpsertAndGetSegment makeStore = TestCase $ do
    store <- makeStore
    getSegmentC store "a" >>= (pure Nothing @=?)
    upsertSegmentC store "a" (Versioned (pure segment) 52) >>= (pure () @=?)
    getSegmentC store "a" >>= (pure (pure segment) @=?)
  where
    segment = makeTestSegment "a" 52

testUpsertRespectsVersion :: IO (StoreHandle IO) -> Test
testUpsertRespectsVersion makeStore = TestCase $ do
    store <- makeStore
    upsertFlagC store "a" (Versioned (pure $ makeTestFlag "a" 1) 1) >>= (pure () @=?)
    upsertFlagC store "a" (Versioned (pure $ makeTestFlag "a" 2) 2) >>= (pure () @=?)
    getFlagC store "a" >>= (pure (pure $ makeTestFlag "a" 2) @=?)
    getAllFlagsC store >>= (pure (singleton "a" $ makeTestFlag "a" 2) @=?)
    upsertFlagC store "a" (Versioned (pure $ makeTestFlag "a" 1) 1) >>= (pure () @=?)
    getFlagC store "a" >>= (pure (pure $ makeTestFlag "a" 2) @=?)
    getAllFlagsC store >>= (pure (singleton "a" $ makeTestFlag "a" 2) @=?)
    upsertFlagC store "a" (Versioned Nothing 3) >>= (pure () @=?)
    getFlagC store "a" >>= (pure Nothing @=?)
    getAllFlagsC store >>= (pure mempty @=?)

testWithStore :: IO (StoreHandle IO) -> Test
testWithStore makeStore =
    TestList $
        map
            (\f -> f makeStore)
            [ testInitializationEmpty
            , testInitializationWithFeatures
            , testGetAndUpsertAndGetAndGetAllFlags
            , testUpsertRespectsVersion
            , testGetAndUpsertAndGetSegment
            ]

allTests :: Test
allTests =
    TestList
        [ testWithStore $ makeStoreIO Nothing 0
        ]
