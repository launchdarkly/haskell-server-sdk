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
import System.Timeout (timeout)
import Test.HUnit

import LaunchDarkly.AesonCompat (KeyMap, lookupKey)
import LaunchDarkly.Server.Config.ClientContext (ClientContext (..))
import LaunchDarkly.Server.Config.HttpConfiguration (HttpConfiguration (..))
import LaunchDarkly.Server.DataSource.Internal (DataSourceUpdates (..))
import LaunchDarkly.Server.Features (Flag)
import LaunchDarkly.Server.Network.Streaming (backoffCeiling, streamingThread)
import Util.Features (makeTestFlag)

allTests :: Test
allTests =
    TestList
        [ TestLabel "healthy stream initializes" testHealthyStream
        , TestLabel "reconnects after an HttpException mid-body" testReconnectAfterHttpException
        , TestLabel "reconnects after a socket reset mid-body" testReconnectAfterSocketReset
        , TestLabel "decodes UTF-8 split across reads" testUtf8SplitAcrossReads
        , TestLabel "killThread stops the streaming thread" testKillThreadStopsStream
        , TestLabel "backoff stays at the cap after many attempts" testBackoffCeiling
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
    , logs :: [(LogLevel, Text)]
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

-- | Runs 'streamingThread' against the scripts until it initializes or exits (at most 5 s), then kills it.
runStream :: [ConnectionScript] -> IO StreamRun
runStream scripts = do
    pending <- newIORef scripts
    logRef <- newIORef []
    initVar <- newEmptyMVar
    settled <- newEmptyMVar
    done <- newEmptyMVar
    manager <- scriptedManager pending
    let logTo _ _ level msg = atomicModifyIORef' logRef (\xs -> ((level, decodeUtf8With lenientDecode $ fromLogStr msg) : xs, ()))
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
                { dataSourceUpdatesInit = \flags _ -> tryPutMVar initVar flags >> tryPutMVar settled () >> pure (Right ())
                , dataSourceUpdatesInsertFlag = const $ pure (Right ())
                , dataSourceUpdatesInsertSegment = const $ pure (Right ())
                , dataSourceUpdatesDeleteFlag = \_ _ -> pure (Right ())
                , dataSourceUpdatesDeleteSegment = \_ _ -> pure (Right ())
                , dataSourceUpdatesSetStatus = const $ pure ()
                }
    thread <-
        forkFinally
            (runLogger context $ streamingThread "http://localhost" 1 context updates)
            (\result -> putMVar done result >> void (tryPutMVar settled ()))
    void $ timeout 5_000_000 $ readMVar settled
    killThread thread
    exited <- timeout 1_000_000 $ readMVar done
    initialized <- tryReadMVar initVar
    remaining <- readIORef pending
    logs <- reverse <$> readIORef logRef
    pure StreamRun {connections = length scripts - length remaining, initialized, logs, exited}

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

testHealthyStream :: Test
testHealthyStream = TestCase $ do
    run <- runStream [healthyConnection]
    assertInitialized run
    assertEqual "connections" 1 (connections run)

testReconnectAfterHttpException :: Test
testReconnectAfterHttpException = TestCase $ do
    run <- runStream [[responseHead, Send "zz\r\n"], healthyConnection]
    assertInitialized run
    assertEqual "connections" 2 (connections run)

testReconnectAfterSocketReset :: Test
testReconnectAfterSocketReset = TestCase $ do
    run <- runStream [[responseHead, chunk beforeAccent, Fail $ toException $ userError "Connection reset by peer"], healthyConnection]
    assertInitialized run
    assertEqual "connections" 2 (connections run)
    assertBool "logs the read error" $
        any (\(level, msg) -> level == LevelError && T.isPrefixOf "streaming connection failed, will retry" msg) (logs run)

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
    assertBool ("thread stopped by ThreadKilled, got: " ++ show (exited run)) $
        case exited run of
            Just (Left e) -> fromException e == Just ThreadKilled
            _ -> False

testBackoffCeiling :: Test
testBackoffCeiling = TestCase $ do
    assertEqual "first attempt waits the initial delay" 1_000_000 (backoffCeiling 1000 1)
    assertEqual "attempt 46 stays at the 30 s cap" 30_000_000 (backoffCeiling 1000 46)
    assertEqual "an initial delay too large for Int arithmetic stays at the 30 s cap" 30_000_000 (backoffCeiling maxBound 1)
