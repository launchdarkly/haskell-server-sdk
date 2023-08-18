module Spec.Integrations.FileData (allTests)
where

import Test.HUnit

import Control.Exception
import Data.Generics.Product (getField)
import qualified Data.HashSet as HS
import LaunchDarkly.AesonCompat (emptyObject)
import LaunchDarkly.Server
import LaunchDarkly.Server.Client.Internal
import LaunchDarkly.Server.DataSource.Internal
import LaunchDarkly.Server.Features (Segment (..))
import LaunchDarkly.Server.Integrations.FileData
import LaunchDarkly.Server.Store.Internal (storeHandleGetSegment)

allTests :: Test
allTests =
    TestList
        [ TestLabel "testAllProperties" testAllProperties
        , TestLabel "testTargettingJson" testTargettingJson
        , TestLabel "testTargettingYaml" testTargettingYaml
        , TestLabel "testMultiFileFlagDuplicate" testMultiFileFlagDuplicate
        , TestLabel "testMalformedFile" testMalformedFile
        , TestLabel "testNoDataFile" testNoDataFile
        , TestLabel "testSegmentFile" testSegmentFile
        , TestLabel "testAllPropertiesYaml" testAllPropertiesYaml
        ]

withClient :: Config -> (Client -> IO a) -> IO a
withClient config = bracket (makeClient config) close

testConfig :: DataSourceFactory -> Config
testConfig factory =
    configSetSendEvents False $
        configSetDataSourceFactory (Just factory) $
            configSetLoggerFilteringDebug $
                makeConfig "sdk-key"

testAllProperties :: Test
testAllProperties = TestCase $ do
    let factory = dataSourceFactory ["test-data/filesource/all-properties.json"]
        config = testConfig factory
        user1 = makeContext "user1" "user"
        user2 = makeContext "user2" "user"
    withClient config $ \client -> do
        status <- getStatus client
        assertEqual "status initialized" Initialized status

        flag1 <- stringVariation client "flag1" user1 "fallback"
        assertEqual "flag1 value" "on" flag1

        flag2 <- stringVariation client "flag2" user1 "fallback"
        assertEqual "flag2 value" "value2" flag2

testTargettingJson :: Test
testTargettingJson = TestCase $ do
    let factory = dataSourceFactory ["test-data/filesource/targets.json"]
        config = testConfig factory
        user1 = makeContext "user1" "user"
        user2 = makeContext "user2" "user"
        org1 = makeContext "org1" "org"
        org2 = makeContext "org2" "org"
    withClient config $ \client -> do
        status <- getStatus client
        assertEqual "status initialized" Initialized status

        flag1 <- stringVariation client "flag1" user1 "fall"
        assertEqual "flag1 user1" "user" flag1

        flag1 <- stringVariation client "flag1" user2 "fall"
        assertEqual "flag1 user2" "fall" flag1

        flag1 <- stringVariation client "flag1" org1 "fall"
        assertEqual "flag1 org1" "org" flag1

        flag1 <- stringVariation client "flag1" org2 "fall"
        assertEqual "flag1 org2" "fall" flag1

testTargettingYaml :: Test
testTargettingYaml = TestCase $ do
    let factory = dataSourceFactory ["test-data/filesource/targets.yml"]
        config = testConfig factory
        user1 = makeContext "user1" "user"
        user2 = makeContext "user2" "user"
        org1 = makeContext "org1" "org"
        org2 = makeContext "org2" "org"
    withClient config $ \client -> do
        status <- getStatus client
        assertEqual "status initialized" Initialized status

        flag1 <- stringVariation client "flag1" user1 "fall"
        assertEqual "flag1 user1" "user" flag1

        flag1 <- stringVariation client "flag1" user2 "fall"
        assertEqual "flag1 user2" "fall" flag1

        flag1 <- stringVariation client "flag1" org1 "fall"
        assertEqual "flag1 org1" "org" flag1

        flag1 <- stringVariation client "flag1" org2 "fall"
        assertEqual "flag1 org2" "fall" flag1

testMultiFileFlagDuplicate :: Test
testMultiFileFlagDuplicate = TestCase $ do
    let factory =
            dataSourceFactory
                [ "test-data/filesource/flag-only.json"
                , "test-data/filesource/flag-with-duplicate-key.json"
                ]
        config = testConfig factory
        user1 = makeContext "user1" "user"
    withClient config $ \client -> do
        status <- getStatus client
        assertEqual "status initialized" Initialized status

        flag1 <- stringVariation client "flag1" user1 "fallback"
        assertEqual "flag1 value" "on" flag1

        anotherFlag <- stringVariation client "another" user1 "fallback"
        assertEqual "flag2 value" "fall" anotherFlag

testMalformedFile :: Test
testMalformedFile = TestCase $ do
    let factory = dataSourceFactory ["test-data/filesource/malformed.json"]
        config = testConfig factory
        user1 = makeContext "user1" "user"
    withClient config $ \client -> do
        (\state -> assertEqual "No Flags set" emptyObject (getField @"evaluations" state))
            =<< allFlagsState client user1 False False False

testNoDataFile :: Test
testNoDataFile = TestCase $ do
    let factory = dataSourceFactory ["test-data/filesource/no-data.json"]
        config = testConfig factory
        user1 = makeContext "user1" "user"
    withClient config $ \client -> do
        (\state -> assertEqual "No Flags set" emptyObject (getField @"evaluations" state))
            =<< allFlagsState client user1 False False False

testSegmentFile :: Test
testSegmentFile = TestCase $ do
    let factory = dataSourceFactory ["test-data/filesource/segment-only.json"]
        config = testConfig factory
    withClient config $ \client -> do
        eSeg1 <- storeHandleGetSegment (store client) "seg1"
        mSeg1 <- either (assertFailure . show) pure eSeg1
        seg1 <- maybe (assertFailure "Segment Not Found") pure mSeg1
        assertEqual "Segment" (included seg1) (HS.fromList ["user1"])

        eSeg2 <- storeHandleGetSegment (store client) "seg2"
        mSeg2 <- either (assertFailure . show) pure eSeg2
        assertEqual "No Segment" Nothing mSeg2

testAllPropertiesYaml :: Test
testAllPropertiesYaml = TestCase $ do
    let factory = dataSourceFactory ["test-data/filesource/all-properties.yml"]
        config = testConfig factory
        user1 = makeContext "user1" "user"
        user2 = makeContext "user2" "user"
    withClient config $ \client -> do
        status <- getStatus client
        assertEqual "status initialized" Initialized status

        flag1 <- stringVariation client "flag1" user1 "fallback"
        assertEqual "flag1 value" "on" flag1

        flag2 <- stringVariation client "flag2" user1 "fallback"
        assertEqual "flag2 value" "value2" flag2
