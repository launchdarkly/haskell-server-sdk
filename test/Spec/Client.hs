module Spec.Client (allTests) where

import Data.Function ((&))
import Data.Generics.Product (getField)
import System.Timeout (timeout)
import Test.HUnit

import LaunchDarkly.Server.Client
import LaunchDarkly.Server.Config
import LaunchDarkly.Server.Context
import LaunchDarkly.Server.Store.Internal

import Data.Text (Text)

makeEmptyStore :: IO (StoreHandle IO)
makeEmptyStore = do
    handle <- makeStoreIO Nothing 0
    initializeStore handle mempty mempty
    pure handle

testSecureModeHashIsGeneratedCorrectly :: Test
testSecureModeHashIsGeneratedCorrectly = TestCase $ do
    client <- makeTestClient "secret"
    assertEqual "" "aa747c502a898200f9e4fa21bac68136f886a0e27aec70ba06daf2e2a5cb5597" (secureModeHash client userContext)
    assertEqual "" "a045e65c6d23bda4559ed4f2371a2508ce63016ceff58b00aa07b435e2bfedaa" (secureModeHash client orgContext)
  where
    userContext = makeContext "Message" "user"
    orgContext = makeContext "Message" "org"

-- Test that flushEvents returns when no event thread takes the flush signal.
testFlushEventsDoesNotBlock :: Test
testFlushEventsDoesNotBlock = TestCase $ do
    client <- makeTestClient "secret"
    result <- timeout (5 * 1000000) $ flushEvents client >> flushEvents client
    assertEqual "flushEvents returned" (Just ()) result

makeTestClient :: Text -> IO Client
makeTestClient sdkKey = do
    client <- makeClient $ (makeConfig sdkKey) & configSetOffline True
    initializeStore (getField @"store" client) mempty mempty
    pure client

allTests :: Test
allTests =
    TestList
        [ testSecureModeHashIsGeneratedCorrectly
        , testFlushEventsDoesNotBlock
        ]
