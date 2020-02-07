module Spec.StoreInterface (allTests) where

import           Control.Monad                      (void)
import           Data.Function                      ((&))
import           Data.IORef                         (newIORef, readIORef, atomicModifyIORef', writeIORef)
import           Data.Either                        (isLeft)
import           Data.HashMap.Strict                (HashMap)
import qualified Data.HashMap.Strict as             HM
import           Data.ByteString                    ()
import           Test.HUnit
import           System.Clock                       (TimeSpec(..))

import           Util.Features                      (makeTestFlag)

import           LaunchDarkly.Server.Store.Internal

makeTestStore :: Maybe StoreInterface -> IO (StoreHandle IO)
makeTestStore backend = makeStoreIO backend $ TimeSpec 10 0

makeStoreInterface :: StoreInterface
makeStoreInterface = StoreInterface
    { storeInterfaceAllFeatures   = undefined
    , storeInterfaceGetFeature    = undefined
    , storeInterfaceUpsertFeature = undefined
    , storeInterfaceIsInitialized = undefined
    , storeInterfaceInitialize    = undefined
    }

testFailInit :: Test
testFailInit = TestCase $ do
    store <- makeTestStore $ pure $ makeStoreInterface
        { storeInterfaceInitialize = \_ -> pure $ Left "err"
        }
    initializeStore store HM.empty HM.empty >>= (Left "err" @?=)

testFailGet :: Test
testFailGet = TestCase $ do
    store <- makeTestStore $ pure $ makeStoreInterface
        { storeInterfaceGetFeature = \_ _ -> pure $ Left "err"
        }
    getFlagC store "abc" >>= (Left "err" @?=)

testFailAll :: Test
testFailAll = TestCase $ do
    store <- makeTestStore $ pure $ makeStoreInterface
        { storeInterfaceAllFeatures = \_ -> pure $ Left "err"
        }
    getAllFlagsC store >>= (Left "err" @?=)

testFailIsInitialized :: Test
testFailIsInitialized = TestCase $ do
    store <- makeTestStore $ pure $ makeStoreInterface
        { storeInterfaceIsInitialized = pure $ Left "err"
        }
    getInitializedC store >>= (Left "err" @?=)

testFailUpsert :: Test
testFailUpsert = TestCase $ do
    store <- makeTestStore $ pure $ makeStoreInterface
        { storeInterfaceUpsertFeature = \_ _ _ -> pure $ Left "err"
        }
    insertFlag store (makeTestFlag "test" 123) >>= (Left "err" @?=)

testFailGetInvalidJSON :: Test
testFailGetInvalidJSON = TestCase $ do
    store <- makeTestStore $ pure $ makeStoreInterface
        { storeInterfaceGetFeature = \_ _ -> pure $ Right $ RawFeature (pure "invalid json") 0
        }
    getFlagC store "abc" >>= (\v -> True @?= isLeft v)

testGetAllInvalidJSON :: Test
testGetAllInvalidJSON = TestCase $ do
    let flag = makeTestFlag "abc" 52
    store <- makeTestStore $ pure $ makeStoreInterface
        { storeInterfaceAllFeatures = \_ -> pure $ Right $ HM.empty
            & HM.insert "abc" (versionedToRaw $ Versioned (pure flag) 52)
            & HM.insert "xyz" (RawFeature (pure "invalid json") 64)
        }
    getAllFlagsC store >>= (Right (HM.singleton "abc" flag) @?=)

testInitializedCache :: Test
testInitializedCache = TestCase $ do
    counter <- newIORef 0
    value   <- newIORef False
    store   <- makeTestStore $ pure $ makeStoreInterface
        { storeInterfaceIsInitialized = do
            atomicModifyIORef' counter (\c -> (c + 1, ()))
            Right <$> readIORef value
        }
    getInitializedC store      >>= (Right False @=?)
    readIORef counter          >>= (1 @=?)
    getInitializedC store      >>= (Right False @=?)
    readIORef counter          >>= (1 @=?)
    storeHandleExpireAll store >>= (Right () @=?)
    getInitializedC store      >>= (Right False @=?)
    readIORef counter          >>= (2 @=?)
    writeIORef value True
    storeHandleExpireAll store >>= (Right () @=?)
    getInitializedC store      >>= (Right True @=?)
    readIORef counter          >>= (3 @=?)
    getInitializedC store      >>= (Right True @=?)
    readIORef counter          >>= (3 @=?)

testGetCache :: Test
testGetCache = TestCase $ do
    counter <- newIORef 0
    value   <- newIORef $ RawFeature Nothing 0
    store   <- makeTestStore $ pure $ makeStoreInterface
        { storeInterfaceGetFeature = \_ _ -> do
            atomicModifyIORef' counter (\c -> (c + 1, ()))
            Right <$> readIORef value
        }
    getFlagC store "abc"       >>= (Right Nothing @?=)
    readIORef counter          >>= (1 @=?)
    getFlagC store "abc"       >>= (Right Nothing @?=)
    readIORef counter          >>= (1 @=?)
    storeHandleExpireAll store >>= (Right () @=?)
    let flag = pure $ makeTestFlag "abc" 12
    writeIORef value $ versionedToRaw $ Versioned flag 12
    getFlagC store "abc"       >>= (Right flag @=?)
    readIORef counter          >>= (2 @=?)
    getFlagC store "abc"       >>= (Right flag @=?)
    readIORef counter          >>= (2 @=?)

testUpsertInvalidatesAllFlags :: Test
testUpsertInvalidatesAllFlags = TestCase $ do
    allCounter    <- newIORef 0
    upsertCounter <- newIORef 0
    upsertResult  <- newIORef $ Right True
    store         <- makeTestStore $ pure $ makeStoreInterface
        { storeInterfaceUpsertFeature = \_ _ _ -> do
            atomicModifyIORef' upsertCounter (\c -> (c + 1, ()))
            readIORef upsertResult
        , storeInterfaceAllFeatures = \_ -> do
            atomicModifyIORef' allCounter (\c -> (c + 1, ()))
            pure $ Right HM.empty
        }
    getAllFlagsC store        >>= (Right HM.empty @=?)
    readIORef allCounter      >>= (1 @=?)
    deleteFlag store "abc" 52 >>= (Right () @=?)
    readIORef upsertCounter   >>= (1 @=?)
    getAllFlagsC store        >>= (Right HM.empty @=?)
    readIORef allCounter      >>= (2 @=?)
    writeIORef upsertResult $ Right False
    deleteFlag store "abc" 53 >>= (Right () @=?)
    readIORef upsertCounter   >>= (2 @=?)
    getAllFlagsC store        >>= (Right HM.empty @=?)
    readIORef allCounter      >>= (2 @=?)

testAllFlagsCache :: Test
testAllFlagsCache = TestCase $ do
    counter <- newIORef 0
    value   <- newIORef HM.empty
    store   <- makeTestStore $ pure $ makeStoreInterface
        { storeInterfaceAllFeatures = \_ -> do
            atomicModifyIORef' counter (\c -> (c + 1, ()))
            pure $ Right HM.empty
        }
    getAllFlagsC store         >>= (Right HM.empty @=?)
    readIORef counter          >>= (1 @=?)
    getAllFlagsC store         >>= (Right HM.empty @=?)
    readIORef counter          >>= (1 @=?)
    storeHandleExpireAll store >>= (Right () @=?)
    getAllFlagsC store         >>= (Right HM.empty @=?)
    readIORef counter          >>= (2 @=?)

testAllFlagsUpdatesRegularCache :: Test
testAllFlagsUpdatesRegularCache = TestCase $ do
    let flag = makeTestFlag "abc" 12
    store <- makeTestStore $ pure $ makeStoreInterface
        { storeInterfaceAllFeatures = \_ -> pure $ Right $
            HM.singleton "abc" (versionedToRaw $ Versioned (pure flag) 12)
        }
    getAllFlagsC store   >>= (Right (HM.singleton "abc" flag) @=?)
    getFlagC store "abc" >>= (Right (pure flag) @=?)

allTests :: Test
allTests = TestList
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
