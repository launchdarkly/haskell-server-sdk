module Spec.Client (allTests) where

import Control.Concurrent (killThread)
import Control.Concurrent.MVar (readMVar)
import Control.Monad (forM_)
import Data.Aeson (Value (..))
import Data.Function ((&))
import Data.Generics.Product (getField)
import System.Timeout (timeout)
import Test.HUnit

import LaunchDarkly.AesonCompat (objectValues, singleton)
import LaunchDarkly.Server.Client
import LaunchDarkly.Server.Client.Internal (Status (Initialized), setStatus)
import LaunchDarkly.Server.Config
import LaunchDarkly.Server.Context
import LaunchDarkly.Server.Events (EventType (..), processSummary)
import LaunchDarkly.Server.Features (Flag (..), VariationOrRollout (..))
import LaunchDarkly.Server.Store.Internal

import Data.Text (Text)
import Util.Features (makeTestFlag)

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

-- Test that a typed evaluation of the wrong type records the default value in the events.
-- The feature event and the summary counter must not record the value or variation of the flag.
testWrongTypeEventRecordsDefaultValue :: Test
testWrongTypeEventRecordsDefaultValue = TestCase $ do
    let config =
            makeConfig "secret"
                & configSetUseLdd True
                & configSetFlushIntervalSeconds 3600
                & configSetEventsURI "http://127.0.0.1:1"
        flag =
            (makeTestFlag "flag" 5)
                { trackEvents = True
                , variations = [Bool True]
                , fallthrough = VariationOrRollout {variation = Just 0, rollout = Nothing}
                }
    client <- makeClient config
    -- Stop the event thread before the evaluation. The thread flushes the event queue when it starts,
    -- and that flush can remove the events that this test reads.
    forM_ (getField @"eventThreadPair" client) $ \(thread, sync) -> killThread thread >> readMVar sync
    initializeStore (getField @"store" client) (singleton "flag" flag) mempty
    setStatus client Initialized

    detail <- stringVariationDetail client "flag" (makeContext "user-key" "user") "default"
    assertEqual "returned detail" (EvaluationDetail "default" Nothing (EvaluationReasonError EvalErrorWrongType)) detail

    let state = getField @"events" client
    processSummary config state
    events <- readMVar $ getField @"events" state
    case [getField @"event" e | EventTypeFeature e <- events] of
        [feature] -> do
            assertEqual "feature event value" (String "default") (getField @"value" feature)
            assertEqual "feature event variation" Nothing (getField @"variation" feature)
            assertEqual "feature event reason" (Just $ EvaluationReasonError EvalErrorWrongType) (getField @"reason" feature)
        other -> assertFailure $ "expected one feature event, got " ++ show (length other)
    case [counter | EventTypeSummary summary <- events, flagSummary <- objectValues (getField @"features" summary), counter <- objectValues (getField @"counters" flagSummary)] of
        [counter] -> do
            assertEqual "counter value" (String "default") (getField @"value" counter)
            assertEqual "counter variation" Nothing (getField @"variation" counter)
            assertEqual "counter version" (Just 5) (getField @"version" counter)
        other -> assertFailure $ "expected one summary counter, got " ++ show (length other)

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
        , testWrongTypeEventRecordsDefaultValue
        ]
