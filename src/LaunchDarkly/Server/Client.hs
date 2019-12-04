-- | This module contains the core functionality of the SDK.

module LaunchDarkly.Server.Client
    ( Client
    , makeClient
    , clientVersion
    , boolVariation
    , boolVariationDetail
    , stringVariation
    , stringVariationDetail
    , intVariation
    , intVariationDetail
    , doubleVariation
    , doubleVariationDetail
    , jsonVariation
    , jsonVariationDetail
    , EvaluationDetail(..)
    , EvaluationReason(..)
    , EvalErrorKind(..)
    , allFlags
    , flushEvents
    , identify
    , track
    , Status(..)
    , getStatus
    ) where

import           Control.Concurrent.MVar               (putMVar)
import           Control.Concurrent                    (forkIO)
import           Control.Monad                         (void, liftM)
import           Data.IORef                            (newIORef, readIORef)
import           Data.HashMap.Strict                   (HashMap)
import qualified Data.HashMap.Strict as                HM
import           Data.Text                             (Text)
import           Data.Aeson                            (Value(..))
import           Data.Generics.Product                 (getField)
import           Data.Scientific                       (toRealFloat, fromFloatDigits)
import           Data.Maybe                            (isNothing)
import           Network.HTTP.Client                   (newManager)
import           Network.HTTP.Client.TLS               (tlsManagerSettings)

import           LaunchDarkly.Server.Config.Internal   (Config(..))
import           LaunchDarkly.Server.Client.Internal
import           LaunchDarkly.Server.User.Internal     (User(..), userSerializeRedacted)
import           LaunchDarkly.Server.Details           (EvaluationDetail(..), EvaluationReason(..), EvalErrorKind(..))
import           LaunchDarkly.Server.Events            (IdentifyEvent(..), CustomEvent(..), EventType(..), makeBaseEvent, queueEvent, makeEventState, addUserToEvent)
import           LaunchDarkly.Server.Network.Eventing  (eventThread)
import           LaunchDarkly.Server.Network.Streaming (streamingThread)
import           LaunchDarkly.Server.Network.Polling   (pollingThread)
import           LaunchDarkly.Server.Store             (getAllFlags)
import           LaunchDarkly.Server.Store.Memory      (makeMemoryStoreIO)
import           LaunchDarkly.Server.Evaluate          (evaluateTyped, evaluateDetail)

-- | Create a new instance of the LaunchDarkly client.
makeClient :: Config -> IO Client
makeClient (Config config) = do
    let runLogger = getField @"logger" config

    status  <- newIORef Uninitialized
    store   <- case getField @"store" config of Nothing -> makeMemoryStoreIO; Just x -> pure x
    manager <- newManager tlsManagerSettings
    events  <- makeEventState config

    let client = ClientI {..}

    void $ forkIO $ runLogger $ eventThread manager client

    void $ forkIO $ runLogger $ if getField @"streaming" config
        then streamingThread manager client
        else pollingThread   manager client

    pure $ Client client

-- | Return the initialization status of the Client
getStatus :: Client -> IO Status
getStatus (Client client) = readIORef $ getField @"status" client

-- | Returns a map from feature flag keys to values for a given user. If the
-- result of the flag's evaluation would result in the default value, `nil` will
-- be returned. This method does not send analytics events back to LaunchDarkly.
allFlags :: Client -> User -> IO (HashMap Text Value)
allFlags (Client client) (User user) = if isNothing $ getField @"key" user then pure mempty else do
    flags <- getAllFlags $ getField @"store" client
    evals <- mapM (\flag -> evaluateDetail flag user $ getField @"store" client) flags
    pure $ HM.map (getField @"value" . fst) evals

-- | Identify reports details about a a user.
identify :: Client -> User -> IO ()
identify (Client client) (User user) = do
    let user' = userSerializeRedacted (getField @"config" client) user
    x <- makeBaseEvent $ IdentifyEvent { key = getField @"key" user, user = user' }
    queueEvent (getField @"config" client) (getField @"events" client) (EventTypeIdentify x)

-- | Track reports that a user has performed an event. Custom data can be
-- attached to the event, and / or a numeric value.
--
-- The numeric value is used by the LaunchDarkly experimentation feature in
-- numeric custom metrics, and will also be returned as part of the custom event
-- for Data Export.
track :: Client -> User -> Text -> Maybe Value -> Maybe Double -> IO ()
track (Client client) (User user) key value metric = do
    x <- makeBaseEvent $ addUserToEvent (getField @"config" client) user CustomEvent
        { key = key, user = Nothing, userKey = Nothing, metricValue = metric, value = value }
    queueEvent (getField @"config" client) (getField @"events" client) (EventTypeCustom x)

-- | Flush tells the client that all pending analytics events (if any) should be
-- delivered as soon as possible. Flushing is asynchronous, so this method will
-- return before it is complete.
flushEvents :: Client -> IO ()
flushEvents (Client client) = putMVar (getField @"flush" $ getField @"events" client) ()

type ValueConverter a = (a -> Value, Value -> Maybe a)

reorderStuff :: ValueConverter a -> Bool -> Client -> Text -> User -> a -> IO (EvaluationDetail a)
reorderStuff converter includeReason (Client client) key (User user) fallback = evaluateTyped client key user fallback (fst converter) includeReason (snd converter)

dropReason :: (Text -> User -> a -> IO (EvaluationDetail a)) -> Text -> User -> a -> IO a
dropReason = (((liftM (getField @"value") .) .) .)

boolConverter :: ValueConverter Bool
boolConverter = (,) Bool $ \case Bool x -> pure x; _ -> Nothing

stringConverter :: ValueConverter Text
stringConverter = (,) String $ \case String x -> pure x; _ -> Nothing

intConverter :: ValueConverter Int
intConverter = (,) (Number . fromIntegral) $ \case Number x -> pure $ truncate x; _ -> Nothing

doubleConverter :: ValueConverter Double
doubleConverter = (,) (Number . fromFloatDigits) $ \case Number x -> pure $ toRealFloat x; _ -> Nothing

jsonConverter :: ValueConverter Value
jsonConverter = (,) id pure

-- | Evaluate a Boolean typed flag.
boolVariation :: Client -> Text -> User -> Bool -> IO Bool
boolVariation = dropReason . reorderStuff boolConverter False

-- | Evaluate a Boolean typed flag, and return an explation.
boolVariationDetail :: Client -> Text -> User -> Bool -> IO (EvaluationDetail Bool)
boolVariationDetail = reorderStuff boolConverter True

-- | Evaluate a String typed flag.
stringVariation :: Client -> Text -> User -> Text -> IO Text
stringVariation = dropReason . reorderStuff stringConverter False

-- | Evaluate a String typed flag, and return an explanation.
stringVariationDetail :: Client -> Text -> User -> Text -> IO (EvaluationDetail Text)
stringVariationDetail = reorderStuff stringConverter True

-- | Evaluate a Number typed flag, and truncate the result.
intVariation :: Client -> Text -> User -> Int -> IO Int
intVariation = dropReason . reorderStuff intConverter False

-- | Evaluate a Number typed flag, truncate the result, and return an
-- explanation.
intVariationDetail :: Client -> Text -> User -> Int -> IO (EvaluationDetail Int)
intVariationDetail = reorderStuff intConverter True

-- | Evaluate a Number typed flag.
doubleVariation :: Client -> Text -> User -> Double -> IO Double
doubleVariation = dropReason . reorderStuff doubleConverter False

-- | Evaluate a Number typed flag, and return an explanation.
doubleVariationDetail :: Client -> Text -> User -> Double -> IO (EvaluationDetail Double)
doubleVariationDetail = reorderStuff doubleConverter True

-- | Evaluate a JSON typed flag.
jsonVariation :: Client -> Text -> User -> Value -> IO Value
jsonVariation = dropReason . reorderStuff jsonConverter False

-- | Evaluate a JSON typed flag, and return an explanation.
jsonVariationDetail :: Client -> Text -> User -> Value -> IO (EvaluationDetail Value)
jsonVariationDetail = reorderStuff jsonConverter True
