{-# LANGUAGE NumericUnderscores #-}

module Spec.Polling (allTests) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, readMVar, tryPutMVar)
import Control.Exception (SomeException, throwIO, toException)
import Control.Monad (forever, void)
import Control.Monad.Logger (LogLevel (..), fromLogStr, runLoggingT)
import Data.Aeson (encode, object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Network.HTTP.Client (Manager, defaultManagerSettings, managerRawConnection, newManager, responseTimeoutNone)
import Network.HTTP.Client.Internal (makeConnection)
import System.Timeout (timeout)
import Test.HUnit

import LaunchDarkly.AesonCompat (KeyMap, objectKeys)
import LaunchDarkly.Server.Config.ClientContext (ClientContext (..))
import LaunchDarkly.Server.Config.HttpConfiguration (HttpConfiguration (..))
import LaunchDarkly.Server.DataSource.Internal (DataSourceUpdates (..))
import LaunchDarkly.Server.Features (Flag)
import LaunchDarkly.Server.Network.Polling (pollingThread)
import Util.Features (makeTestFlag)

allTests :: Test
allTests =
    TestList
        [ TestLabel "polls again after a connection reset during the response body" testPollsAgainAfterBodyReset
        ]

-- | One socket read. 'Fail' throws from inside the read, the way a reset socket does.
data ReadStep = Send ByteString | Fail SomeException

-- | Serves each new connection from the next list of reads. Once the lists run out, opening a connection blocks.
-- Once the reads of a connection run out, reads block.
scriptedManager :: IORef [[ReadStep]] -> IO Manager
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

-- | A poll response with one flag.
pollBody :: ByteString
pollBody = L.toStrict $ encode $ object ["flags" .= object ["flag" .= makeTestFlag "flag" 1], "segments" .= object []]

responseHead :: ByteString
responseHead = "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: " <> B8.pack (show $ B.length pollBody) <> "\r\n\r\n"

-- The first poll gets half the body and then a socket reset. The second poll gets the full body.
-- The poll interval is 0 s, so the second poll starts as soon as the first one fails.
testPollsAgainAfterBodyReset :: Test
testPollsAgainAfterBodyReset = TestCase $ do
    pending <- newIORef [[Send responseHead, Send (B.take (B.length pollBody `div` 2) pollBody), Fail reset], [Send responseHead, Send pollBody]]
    manager <- scriptedManager pending
    logRef <- newIORef []
    initVar <- newEmptyMVar
    let logTo _ _ level msg = atomicModifyIORef' logRef (\xs -> (xs ++ [(level, decodeUtf8With lenientDecode $ fromLogStr msg)], ()))
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
                { dataSourceUpdatesInit = \flags _ -> void (tryPutMVar initVar flags) >> pure (Right ())
                , dataSourceUpdatesInsertFlag = const $ pure (Right ())
                , dataSourceUpdatesInsertSegment = const $ pure (Right ())
                , dataSourceUpdatesDeleteFlag = \_ _ -> pure (Right ())
                , dataSourceUpdatesDeleteSegment = \_ _ -> pure (Right ())
                , dataSourceUpdatesSetStatus = const $ pure ()
                }
    thread <- forkIO $ runLogger context $ pollingThread "http://localhost" 0 context updates
    initialized <- timeout 5_000_000 (readMVar initVar :: IO (KeyMap Flag))
    killThread thread
    logs <- readIORef logRef
    assertEqual "flags from the second poll" (Just ["flag"]) (objectKeys <$> initialized)
    assertBool ("logs the reset, got: " ++ show logs) $
        any (\(level, msg) -> level == LevelError && T.isInfixOf "Connection reset by peer" msg) logs
  where
    reset = toException $ userError "Connection reset by peer"
