module Spec.PersistentDataStore (allTests) where

import Data.ByteString ()
import Data.Either (isLeft)
import Data.Function ((&))
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import System.Clock (TimeSpec (..))
import Test.HUnit

import Util.Features (makeTestFlag)

import LaunchDarkly.AesonCompat (emptyObject, insertKey, singleton)
import LaunchDarkly.Server.Store.Internal

makeTestStore :: Maybe PersistentDataStore -> IO (StoreHandle IO)
makeTestStore backend = makeStoreIO backend $ TimeSpec 10 0

makeStoreInterface :: PersistentDataStore
makeStoreInterface =
    PersistentDataStore
        { persistentDataStoreAllFeatures = const $ assertFailure "allFeatures should not be called"
        , persistentDataStoreGetFeature = const $ const $ assertFailure "getFeatures should not be called"
        , persistentDataStoreUpsertFeature = const $ const $ const $ assertFailure "upsertFeature should not be called"
        , persistentDataStoreIsInitialized = assertFailure "isInitialized should not be called"
        , persistentDataStoreInitialize = const $ assertFailure "initialize should not be called"
        }

testFailInit :: Test
testFailInit = TestCase $ do
    store <-
        makeTestStore $
            pure $
                makeStoreInterface
                    { persistentDataStoreInitialize = \_ -> pure $ Left "err"
                    }
    initializeStore store emptyObject emptyObject >>= (Left "err" @?=)

testFailGet :: Test
testFailGet = TestCase $ do
    store <-
        makeTestStore $
            pure $
                makeStoreInterface
                    { persistentDataStoreGetFeature = \_ _ -> pure $ Left "err"
                    }
    getFlagC store "abc" >>= (Left "err" @?=)

testFailAll :: Test
testFailAll = TestCase $ do
    store <-
        makeTestStore $
            pure $
                makeStoreInterface
                    { persistentDataStoreAllFeatures = \_ -> pure $ Left "err"
                    }
    getAllFlagsC store >>= (Left "err" @?=)

testFailIsInitialized :: Test
testFailIsInitialized = TestCase $ do
    store <-
        makeTestStore $
            pure $
                makeStoreInterface
                    { persistentDataStoreIsInitialized = pure $ Left "err"
                    }
    getInitializedC store >>= (Left "err" @?=)

testFailUpsert :: Test
testFailUpsert = TestCase $ do
    store <-
        makeTestStore $
            pure $
                makeStoreInterface
                    { persistentDataStoreUpsertFeature = \_ _ _ -> pure $ Left "err"
                    }
    insertFlag store (makeTestFlag "test" 123) >>= (Left "err" @?=)

testFailGetInvalidJSON :: Test
testFailGetInvalidJSON = TestCase $ do
    store <-
        makeTestStore $
            pure $
                makeStoreInterface
                    { persistentDataStoreGetFeature = \_ _ -> pure $ Right $ Just $ SerializedItemDescriptor (pure "invalid json") 0 False
                    }
    getFlagC store "abc" >>= (\v -> True @?= isLeft v)

testGetAllInvalidJSON :: Test
testGetAllInvalidJSON = TestCase $ do
    let flag = makeTestFlag "abc" 52
    store <-
        makeTestStore $
            pure $
                makeStoreInterface
                    { persistentDataStoreAllFeatures = \_ ->
                        pure $
                            Right $
                                emptyObject
                                    & insertKey "abc" (createSerializedItemDescriptor $ ItemDescriptor (pure flag) 52)
                                    & insertKey "xyz" (SerializedItemDescriptor (pure "invalid json") 64 False)
                    }
    getAllFlagsC store >>= (Right (singleton "abc" flag) @?=)

testInitializedCache :: Test
testInitializedCache = TestCase $ do
    counter <- newIORef 0
    value <- newIORef False
    store <-
        makeTestStore $
            pure $
                makeStoreInterface
                    { persistentDataStoreIsInitialized = do
                        atomicModifyIORef' counter (\c -> (c + 1, ()))
                        Right <$> readIORef value
                    }
    getInitializedC store >>= (Right False @=?)
    readIORef counter >>= (1 @=?)
    getInitializedC store >>= (Right False @=?)
    readIORef counter >>= (1 @=?)
    storeHandleExpireAll store >>= (Right () @=?)
    getInitializedC store >>= (Right False @=?)
    readIORef counter >>= (2 @=?)
    writeIORef value True
    storeHandleExpireAll store >>= (Right () @=?)
    getInitializedC store >>= (Right True @=?)
    readIORef counter >>= (3 @=?)
    getInitializedC store >>= (Right True @=?)
    readIORef counter >>= (3 @=?)

testGetCache :: Test
testGetCache = TestCase $ do
    counter <- newIORef 0
    value <- newIORef $ Just $ SerializedItemDescriptor Nothing 0 False
    store <-
        makeTestStore $
            pure $
                makeStoreInterface
                    { persistentDataStoreGetFeature = \_ _ -> do
                        atomicModifyIORef' counter (\c -> (c + 1, ()))
                        Right <$> readIORef value
                    }
    getFlagC store "abc" >>= (Right Nothing @?=)
    readIORef counter >>= (1 @=?)
    getFlagC store "abc" >>= (Right Nothing @?=)
    readIORef counter >>= (1 @=?)
    storeHandleExpireAll store >>= (Right () @=?)
    let flag = pure $ makeTestFlag "abc" 12
    writeIORef value $ Just $ createSerializedItemDescriptor $ ItemDescriptor flag 12
    getFlagC store "abc" >>= (Right flag @=?)
    readIORef counter >>= (2 @=?)
    getFlagC store "abc" >>= (Right flag @=?)
    readIORef counter >>= (2 @=?)

testUpsertInvalidatesAllFlags :: Test
testUpsertInvalidatesAllFlags = TestCase $ do
    allCounter <- newIORef 0
    upsertCounter <- newIORef 0
    upsertResult <- newIORef $ Right True
    store <-
        makeTestStore $
            pure $
                makeStoreInterface
                    { persistentDataStoreUpsertFeature = \_ _ _ -> do
                        atomicModifyIORef' upsertCounter (\c -> (c + 1, ()))
                        readIORef upsertResult
                    , persistentDataStoreAllFeatures = \_ -> do
                        atomicModifyIORef' allCounter (\c -> (c + 1, ()))
                        pure $ Right emptyObject
                    }
    getAllFlagsC store >>= (Right emptyObject @=?)
    readIORef allCounter >>= (1 @=?)
    deleteFlag store "abc" 52 >>= (Right () @=?)
    readIORef upsertCounter >>= (1 @=?)
    getAllFlagsC store >>= (Right emptyObject @=?)
    readIORef allCounter >>= (2 @=?)
    writeIORef upsertResult $ Right False
    deleteFlag store "abc" 53 >>= (Right () @=?)
    readIORef upsertCounter >>= (2 @=?)
    getAllFlagsC store >>= (Right emptyObject @=?)
    readIORef allCounter >>= (2 @=?)

testAllFlagsCache :: Test
testAllFlagsCache = TestCase $ do
    counter <- newIORef 0
    value <- newIORef emptyObject
    store <-
        makeTestStore $
            pure $
                makeStoreInterface
                    { persistentDataStoreAllFeatures = \_ -> do
                        atomicModifyIORef' counter (\c -> (c + 1, ()))
                        pure $ Right emptyObject
                    }
    getAllFlagsC store >>= (Right emptyObject @=?)
    readIORef counter >>= (1 @=?)
    getAllFlagsC store >>= (Right emptyObject @=?)
    readIORef counter >>= (1 @=?)
    storeHandleExpireAll store >>= (Right () @=?)
    getAllFlagsC store >>= (Right emptyObject @=?)
    readIORef counter >>= (2 @=?)

testAllFlagsUpdatesRegularCache :: Test
testAllFlagsUpdatesRegularCache = TestCase $ do
    let flag = makeTestFlag "abc" 12
    store <-
        makeTestStore $
            pure $
                makeStoreInterface
                    { persistentDataStoreAllFeatures = \_ ->
                        pure $
                            Right $
                                singleton "abc" (createSerializedItemDescriptor $ ItemDescriptor (pure flag) 12)
                    }
    getAllFlagsC store >>= (Right (singleton "abc" flag) @=?)
    getFlagC store "abc" >>= (Right (pure flag) @=?)

allTests :: Test
allTests =
    TestList
        [ testFailInit
        , testFailGet
        , testFailAll
        , testFailIsInitialized
        , testFailUpsert
        , testFailGetInvalidJSON
        , testGetAllInvalidJSON
        , testInitializedCache
        , testGetCache
        , testUpsertInvalidatesAllFlags
        , testAllFlagsCache
        , testAllFlagsUpdatesRegularCache
        ]
