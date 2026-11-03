module Spec.Events (allTests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Monad (forM, forM_, replicateM_)
import Data.Aeson (Value (..))
import Data.Function ((&))
import Data.Generics.Product (getField)
import GHC.Natural (Natural)
import System.Timeout (timeout)
import Test.HUnit

import LaunchDarkly.AesonCompat (objectValues)
import LaunchDarkly.Server.Config (configSetSendEvents, makeConfig)
import LaunchDarkly.Server.Config.Internal (Config)
import LaunchDarkly.Server.Context (makeContext)
import LaunchDarkly.Server.Details (EvaluationReason (EvaluationReasonOff))
import LaunchDarkly.Server.Events

enabledConfig :: Config
enabledConfig = makeConfig "sdk-key"

disabledConfig :: Config
disabledConfig = makeConfig "sdk-key" & configSetSendEvents False

evalEvent :: EvalEvent
evalEvent = newUnknownFlagEvent "flag" (Bool True) EvaluationReasonOff (makeContext "user-key" "user")

summaryEvents :: EventState -> IO [SummaryEvent]
summaryEvents state = do
    events <- readMVar $ getField @"events" state
    pure [summary | EventTypeSummary summary <- events]

totalCount :: SummaryEvent -> Natural
totalCount summary =
    sum
        [ getField @"count" counter
        | flag <- objectValues $ getField @"features" summary
        , counter <- objectValues $ getField @"counters" flag
        ]

-- Test that a flush with no evaluations does not queue a summary event.
testNoSummaryWithoutEvaluations :: Test
testNoSummaryWithoutEvaluations = TestCase $ do
    state <- makeEventState enabledConfig
    processSummary enabledConfig state
    summaries <- summaryEvents state
    assertEqual "summary event count" 0 (length summaries)

-- Test that evaluations with events disabled do not keep summary data.
-- The flush uses an enabled config to show any data that is kept.
testDisabledEventsKeepNoSummary :: Test
testDisabledEventsKeepNoSummary = TestCase $ do
    state <- makeEventState disabledConfig
    replicateM_ 100 $ processEvalEvents disabledConfig state (makeContext "user-key" "user") False [evalEvent] False
    processSummary enabledConfig state
    summaries <- summaryEvents state
    assertEqual "summary event count" 0 (length summaries)

-- Test that the start date is the earliest evaluation time, not the first one recorded.
testStartDateIsEarliestEvaluation :: Test
testStartDateIsEarliestEvaluation = TestCase $ do
    state <- makeEventState enabledConfig
    runSummary 200 state evalEvent False
    runSummary 100 state evalEvent False
    runSummary 300 state evalEvent False
    processSummary enabledConfig state
    summaries <- summaryEvents state
    case summaries of
        [summary] -> do
            assertEqual "startDate" 100 (getField @"startDate" summary)
            assertEqual "total count" 3 (totalCount summary)
        _ -> assertFailure $ "expected one summary event, got " ++ show (length summaries)

-- Test that a flush resets the summary, so the next window counts only new evaluations.
testFlushStartsNewWindow :: Test
testFlushStartsNewWindow = TestCase $ do
    state <- makeEventState enabledConfig
    runSummary 100 state evalEvent False
    runSummary 100 state evalEvent False
    processSummary enabledConfig state
    runSummary 500 state evalEvent False
    processSummary enabledConfig state
    -- The events queue holds the newest event first.
    summaries <- summaryEvents state
    case summaries of
        [second, first] -> do
            assertEqual "first startDate" 100 (getField @"startDate" first)
            assertEqual "first total count" 2 (totalCount first)
            assertEqual "second startDate" 500 (getField @"startDate" second)
            assertEqual "second total count" 1 (totalCount second)
        _ -> assertFailure $ "expected two summary events, got " ++ show (length summaries)

-- Test that concurrent evaluations finish and that the summary counts every evaluation.
testConcurrentEvaluationsAreCounted :: Test
testConcurrentEvaluationsAreCounted = TestCase $ do
    state <- makeEventState enabledConfig
    let threadCount = 32 :: Int
        callsPerThread = 1000 :: Int
    result <- timeout (30 * 1000000) $ do
        done <- forM [1 .. threadCount] $ \thread -> do
            finished <- newEmptyMVar
            _ <- forkIO $ do
                forM_ [1 .. callsPerThread] $ \call ->
                    runSummary (fromIntegral $ 1000 + thread + call) state evalEvent False
                putMVar finished ()
            pure finished
        mapM_ takeMVar done
    assertEqual "all threads finished" (Just ()) result
    processSummary enabledConfig state
    summaries <- summaryEvents state
    case summaries of
        [summary] -> do
            assertEqual "startDate" 1002 (getField @"startDate" summary)
            assertEqual "total count" (fromIntegral $ threadCount * callsPerThread) (totalCount summary)
        _ -> assertFailure $ "expected one summary event, got " ++ show (length summaries)

allTests :: Test
allTests =
    TestList
        [ testNoSummaryWithoutEvaluations
        , testDisabledEventsKeepNoSummary
        , testStartDateIsEarliestEvaluation
        , testFlushStartsNewWindow
        , testConcurrentEvaluationsAreCounted
        ]
