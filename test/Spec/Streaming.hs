{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Spec.Streaming (allTests) where

import Control.Concurrent (forkFinally, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, tryPutMVar, tryReadMVar)
import Control.Exception (AsyncException (ThreadKilled), SomeException, fromException, throwIO, toException)
import Control.Monad (forever, void)
import Control.Monad.Logger (LogLevel (..), fromLogStr, runLoggingT)
import Data.Aeson (encode, object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Network.HTTP.Client (Manager, defaultManagerSettings, managerRawConnection, newManager, responseTimeoutNone)
import Network.HTTP.Client.Internal (makeConnection)
import Numeric (showHex)
import System.Clock (Clock (Monotonic), TimeSpec (TimeSpec), getTime, toNanoSecs)
import System.Timeout (timeout)
import Test.HUnit

import LaunchDarkly.AesonCompat (KeyMap, lookupKey)
import LaunchDarkly.Server.Config.ClientContext (ClientContext (..))
import LaunchDarkly.Server.Config.HttpConfiguration (HttpConfiguration (..))
import LaunchDarkly.Server.DataSource.Internal (DataSourceUpdates (..))
import LaunchDarkly.Server.Features (Flag)
import LaunchDarkly.Server.Network.Streaming (backoffCeiling, retryAttempt, streamingThread)
import Util.Features (makeTestFlag)

allTests :: Test
allTests =
    TestList
        [ TestLabel "healthy stream initializes and killThread stops it" testKillThreadStopsStream
        , TestLabel "reconnects after an HttpException mid-body" testReconnectAfterHttpException
        , TestLabel "reconnects after a socket reset mid-body" testReconnectAfterSocketReset
        , TestLabel "logs a retried HTTP error status" testLogsRetriedStatus
        , TestLabel "logs a connection failure before the response" testLogsConnectionFailure
        , TestLabel "logs a failure reading an error response body" testLogsErrorBodyFailure
        , TestLabel "logs the delay the stream waits before it retries" testLogsActualDelay
        , TestLabel "logs a retry after the server closes the stream" testLogsCleanClose
        , TestLabel "logs a redirect loop without the responses" testLogsRedirectLoop
        , TestLabel "logs a permanent HTTP error and stops" testLogsPermanentStatus
        , TestLabel "does not retry an unauthorized SDK key" testUnauthorizedDoesNotRetry
        , TestLabel "logs an invalid stream URI and stops" testLogsInvalidUri
        , TestLabel "decodes UTF-8 split across reads" testUtf8SplitAcrossReads
        , TestLabel "backoff stays at the cap after many attempts" testBackoffCeiling
        , TestLabel "a connection open for 60 s resets the backoff" testRetryAttempt
        ]

-- | One socket read. 'Fail' throws from inside the read, the way a reset socket does.
data ReadStep = Send ByteString | Fail SomeException

-- | The reads one TCP connection returns, in order. Once they run out, reads block.
type ConnectionScript = [ReadStep]

data StreamRun = StreamRun
    { connections :: Int
    -- ^ How many scripted connections the SDK opened
    , initialized :: Maybe (KeyMap Flag)
    -- ^ The flags passed to 'dataSourceUpdatesInit', if it was called
    , initializedAt :: Maybe TimeSpec
    -- ^ The monotonic time when 'dataSourceUpdatesInit' was called
    , logs :: [(LogLevel, Text)]
    , logTimes :: [TimeSpec]
    -- ^ The monotonic time of each entry in 'logs'
    , exited :: Maybe (Either SomeException ())
    -- ^ How the streaming thread ended. 'Nothing' when it was still running 1 s after 'killThread'.
    }

-- | Serves each new connection from the next script. Once the scripts run out, opening a connection blocks.
scriptedManager :: IORef [ConnectionScript] -> IO Manager
scriptedManager pending =
    newManager defaultManagerSettings {managerRawConnection = pure $ \_ _ _ -> openNext}
  where
    openNext =
        pop pending >>= \case
            Nothing -> blockForever
            Just script -> do
                steps <- newIORef script
                makeConnection (readStep steps) (const $ pure ()) (pure ())
    readStep steps =
        pop steps >>= \case
            Nothing -> blockForever
            Just (Send bytes) -> pure bytes
            Just (Fail e) -> throwIO e

pop :: IORef [a] -> IO (Maybe a)
pop ref = atomicModifyIORef' ref $ \case
    [] -> ([], Nothing)
    x : rest -> (rest, Just x)

blockForever :: IO a
blockForever = forever $ threadDelay 1_000_000_000

-- | Runs 'streamingThread' against the scripts until it initializes or exits, for at most 5 s, then kills it.
runStream :: [ConnectionScript] -> IO StreamRun
runStream = runStreamAt "http://localhost"

-- | Like 'runStream', with the stream URI given.
runStreamAt :: Text -> [ConnectionScript] -> IO StreamRun
runStreamAt streamURI = runStreamWith streamURI 1

-- | Like 'runStreamAt', with the initial retry delay in milliseconds given.
runStreamWith :: Text -> Int -> [ConnectionScript] -> IO StreamRun
runStreamWith streamURI initialRetryDelay scripts = do
    pending <- newIORef scripts
    logRef <- newIORef []
    initVar <- newEmptyMVar
    settled <- newEmptyMVar
    done <- newEmptyMVar
    manager <- scriptedManager pending
    let logTo _ _ level msg = do
            now <- getTime Monotonic
            atomicModifyIORef' logRef (\xs -> ((now, (level, decodeUtf8With lenientDecode $ fromLogStr msg)) : xs, ()))
        context =
            ClientContext
                { runLogger = (`runLoggingT` logTo)
                , httpConfiguration =
                    HttpConfiguration
                        { defaultRequestHeaders = []
                        , defaultRequestTimeout = responseTimeoutNone
                        , tlsManager = manager
                        }
                }
        updates =
            DataSourceUpdates
                { dataSourceUpdatesInit = \flags _ -> do
                    now <- getTime Monotonic
                    void $ tryPutMVar initVar (flags, now)
                    void $ tryPutMVar settled ()
                    pure (Right ())
                , dataSourceUpdatesInsertFlag = const $ pure (Right ())
                , dataSourceUpdatesInsertSegment = const $ pure (Right ())
                , dataSourceUpdatesDeleteFlag = \_ _ -> pure (Right ())
                , dataSourceUpdatesDeleteSegment = \_ _ -> pure (Right ())
                , dataSourceUpdatesSetStatus = const $ pure ()
                }
    thread <-
        forkFinally
            (runLogger context $ streamingThread streamURI initialRetryDelay context updates)
            (\result -> putMVar done result >> void (tryPutMVar settled ()))
    void $ timeout 5_000_000 $ readMVar settled
    killThread thread
    exited <- timeout 1_000_000 $ readMVar done
    initResult <- tryReadMVar initVar
    remaining <- readIORef pending
    timedLogs <- reverse <$> readIORef logRef
    pure
        StreamRun
            { connections = length scripts - length remaining
            , initialized = fst <$> initResult
            , initializedAt = snd <$> initResult
            , logs = map snd timedLogs
            , logTimes = map fst timedLogs
            , exited
            }

responseHead :: ReadStep
responseHead = Send "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nTransfer-Encoding: chunked\r\n\r\n"

-- | Frames the bytes as one HTTP chunk, which the SDK receives from a single 'brRead'.
chunk :: ByteString -> ReadStep
chunk bytes = Send $ B8.pack (showHex (B.length bytes) "") <> "\r\n" <> bytes <> "\r\n"

-- | A put event whose only flag key, @café@, is the first non-ASCII text in the payload.
putEvent :: ByteString
putEvent = "event: put\ndata: " <> L.toStrict (encode payload) <> "\n\n"
  where
    payload =
        object
            [ "path" .= ("/" :: Text)
            , "data" .= object ["flags" .= object ["café" .= makeTestFlag "café" 1], "segments" .= object []]
            ]

-- | 'putEvent' split at the first byte of the two-byte UTF-8 encoding of @é@ (0xC3 0xA9).
beforeAccent, fromAccent :: ByteString
(beforeAccent, fromAccent) = B.break (== 0xC3) putEvent

healthyConnection :: ConnectionScript
healthyConnection = [responseHead, chunk putEvent]

assertInitialized :: StreamRun -> Assertion
assertInitialized run =
    assertBool
        ("stream never initialized; thread exit: " ++ show (exited run) ++ "; logs: " ++ show (logs run))
        (isJust $ initialized run >>= lookupKey "café")

testReconnectAfterHttpException :: Test
testReconnectAfterHttpException = TestCase $ do
    run <- runStream [[responseHead, Send "zz\r\n"], healthyConnection]
    assertInitialized run
    assertEqual "connections" 2 (connections run)
    assertOneRetryLog run "InvalidChunkHeaders"

-- | The log lines that report a retry or a permanent error.
retryLogs :: StreamRun -> [(LogLevel, Text)]
retryLogs = filter (\(_, msg) -> T.isInfixOf "will retry" msg || T.isInfixOf "will not retry" msg) . logs

-- | Asserts one Warn retry log with the delay and the given reason.
-- The test delay is 1 ms minus up to 0.5 ms of jitter. It prints as 0.001, or as 0.000 at the full 0.5 ms of jitter.
assertOneRetryLog :: StreamRun -> Text -> Assertion
assertOneRetryLog run reason = case retryLogs run of
    [(level, msg)] -> do
        assertEqual "log level" LevelWarn level
        assertBool ("logs the delay and the reason, got: " ++ T.unpack msg) $
            msg `elem` [T.concat ["LaunchDarkly streaming connection was interrupted and will retry in ", delay, " s. Reason: ", reason] | delay <- ["0.000", "0.001"]]
    other -> assertFailure $ "expected one retry log, got: " ++ show other

-- | Asserts one Error log for a permanent error with the given reason, and no retry.
assertPermanentError :: StreamRun -> Text -> Assertion
assertPermanentError run reason = do
    assertEqual "connections" 1 (connections run)
    assertEqual
        "logs"
        [(LevelError, "A permanent error has been encountered with the LaunchDarkly streaming connection. It will not retry. Reason: " <> reason)]
        (retryLogs run)

-- | A response with no body that closes the connection, so the next request opens a new scripted connection.
emptyResponse :: ByteString -> [ByteString] -> ConnectionScript
emptyResponse status headers = [Send $ "HTTP/1.1 " <> status <> "\r\n" <> B.concat (map (<> "\r\n") headers) <> "Content-Length: 0\r\nConnection: close\r\n\r\n"]

testReconnectAfterSocketReset :: Test
testReconnectAfterSocketReset = TestCase $ do
    run <- runStream [[responseHead, chunk beforeAccent, Fail $ toException $ userError "Connection reset by peer"], healthyConnection]
    assertInitialized run
    assertEqual "connections" 2 (connections run)
    assertOneRetryLog run "user error (Connection reset by peer)"

testLogsRetriedStatus :: Test
testLogsRetriedStatus = TestCase $ do
    run <- runStream [emptyResponse "503 Service Unavailable" [], healthyConnection]
    assertInitialized run
    assertEqual "connections" 2 (connections run)
    assertOneRetryLog run "HTTP error 503"

testLogsConnectionFailure :: Test
testLogsConnectionFailure = TestCase $ do
    run <- runStream [[Fail $ toException $ userError "Connection refused"], healthyConnection]
    assertInitialized run
    assertEqual "connections" 2 (connections run)
    assertOneRetryLog run "user error (Connection refused)"

testLogsErrorBodyFailure :: Test
testLogsErrorBodyFailure = TestCase $ do
    run <- runStream [[Send "HTTP/1.1 503 Service Unavailable\r\nContent-Length: 100\r\n\r\nab", Fail $ toException $ userError "reset in body"], healthyConnection]
    assertInitialized run
    assertEqual "connections" 2 (connections run)
    assertOneRetryLog run "user error (reset in body)"

-- The logged delay must be the delay the stream waits, after jitter.
-- The 200 ms initial delay gives a delay of 100 ms to 200 ms. These bounds are whole milliseconds, so the logged delay
-- must be in that range with no allowance for rounding.
-- The stream waits at least the delay before it connects again, so the logged delay must also be no more than the time
-- from the log to initialization. This check allows for the rounding to 1 ms.
-- A log of the delay before jitter fails the second check, except when the jitter is less than about 0.6 ms.
testLogsActualDelay :: Test
testLogsActualDelay = TestCase $ do
    run <- runStreamWith "http://localhost" 200 [emptyResponse "503 Service Unavailable" [], healthyConnection]
    assertInitialized run
    let prefix = "LaunchDarkly streaming connection was interrupted and will retry in "
    case [(time, msg) | (time, (_, msg)) <- zip (logTimes run) (logs run), T.isPrefixOf prefix msg] of
        [(loggedAt, msg)] -> do
            let logged = read (T.unpack $ T.takeWhile (/= ' ') $ T.drop (T.length prefix) msg) :: Double
                waited = maybe 0 (\time -> fromIntegral (toNanoSecs (time - loggedAt)) / 1e9) (initializedAt run)
            assertBool ("logged a delay of " ++ show logged ++ " s, outside 0.1 s to 0.2 s") $ logged >= 0.1 && logged <= 0.2
            assertBool ("logged a delay of " ++ show logged ++ " s but waited " ++ show waited ++ " s") $ waited >= logged - 0.0005
        other -> assertFailure $ "expected one retry log, got: " ++ show other

testLogsCleanClose :: Test
testLogsCleanClose = TestCase $ do
    let closingHead = Send "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nTransfer-Encoding: chunked\r\nConnection: close\r\n\r\n"
    run <- runStream [[closingHead, Send "0\r\n\r\n"], healthyConnection]
    assertInitialized run
    assertEqual "connections" 2 (connections run)
    assertOneRetryLog run "the stream ended"

testLogsRedirectLoop :: Test
testLogsRedirectLoop = TestCase $ do
    let redirect = emptyResponse "302 Found" ["Location: http://localhost/all", "Set-Cookie: session=secret"]
    run <- runStream (replicate 11 redirect ++ [healthyConnection])
    assertInitialized run
    assertEqual "connections" 12 (connections run)
    assertOneRetryLog run "too many redirects"

testLogsPermanentStatus :: Test
testLogsPermanentStatus = TestCase $ do
    run <- runStream [emptyResponse "404 Not Found" [], healthyConnection]
    assertPermanentError run "HTTP error 404"

testUnauthorizedDoesNotRetry :: Test
testUnauthorizedDoesNotRetry = TestCase $ do
    run <- runStream [emptyResponse "401 Unauthorized" [], healthyConnection]
    assertEqual "connections" 1 (connections run)
    assertEqual "retry logs" [] (retryLogs run)

testLogsInvalidUri :: Test
testLogsInvalidUri = TestCase $ do
    run <- runStreamAt "https://user:secret@relay:port" [healthyConnection]
    assertEqual "connections" 0 (connections run)
    assertEqual
        "logs"
        [(LevelError, "A permanent error has been encountered with the LaunchDarkly streaming connection. It will not retry. Reason: invalid stream URI: Invalid URL")]
        (retryLogs run)
    assertBool "no log line contains the password" $ not $ any (T.isInfixOf "secret" . snd) (logs run)

testUtf8SplitAcrossReads :: Test
testUtf8SplitAcrossReads = TestCase $ do
    assertBool "payload contains 0xC3" (not $ B.null fromAccent)
    run <- runStream [[responseHead, chunk beforeAccent, chunk (B.take 1 fromAccent), chunk (B.drop 1 fromAccent)]]
    assertInitialized run
    assertEqual "connections" 1 (connections run)

testKillThreadStopsStream :: Test
testKillThreadStopsStream = TestCase $ do
    run <- runStream [healthyConnection]
    assertInitialized run
    assertEqual "connections" 1 (connections run)
    assertBool ("thread stopped by ThreadKilled, got: " ++ show (exited run)) $
        case exited run of
            Just (Left e) -> fromException e == Just ThreadKilled
            _ -> False

testBackoffCeiling :: Test
testBackoffCeiling = TestCase $ do
    assertEqual "first attempt waits the initial delay" 1_000_000 (backoffCeiling 1000 1)
    assertEqual "attempt 46 stays at the 30 s cap" 30_000_000 (backoffCeiling 1000 46)
    assertEqual "an initial delay too large for Int arithmetic stays at the 30 s cap" 30_000_000 (backoffCeiling maxBound 1)

testRetryAttempt :: Test
testRetryAttempt = TestCase $ do
    assertEqual "a connection that did not open increments the attempt" 7 (retryAttempt 6 Nothing (TimeSpec 1000 0))
    assertEqual "a connection open for less than 60 s increments the attempt" 7 (retryAttempt 6 (Just (TimeSpec 100 0)) (TimeSpec 159 0))
    assertEqual "a connection open for 60 s resets to the first attempt" 1 (retryAttempt 6 (Just (TimeSpec 100 0)) (TimeSpec 160 0))
    assertEqual "a connection open for hours resets to the first attempt" 1 (retryAttempt 6 (Just (TimeSpec 100 0)) (TimeSpec 10_000 0))
