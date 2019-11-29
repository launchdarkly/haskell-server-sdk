module LaunchDarkly.Server
    ( User(..)
    , makeUser
    , Config(..)
    , makeConfig
    , Client
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
    , allFlags
    , flushEvents
    , identify
    , track
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

import           LaunchDarkly.Server.User              (User(..), makeUser, userSerializeRedacted)
import           LaunchDarkly.Server.Config            (Config(..), makeConfig)
import           LaunchDarkly.Server.Client            (Client(..), Status(..), clientVersion)
import           LaunchDarkly.Server.Details           (EvaluationDetail(..), EvaluationReason(..))
import           LaunchDarkly.Server.Events            (IdentifyEvent(..), CustomEvent(..), EventType(..), makeBaseEvent, queueEvent, makeEventState, addUserToEvent)
import           LaunchDarkly.Server.Network.Eventing  (eventThread)
import           LaunchDarkly.Server.Network.Streaming (streamingThread)
import           LaunchDarkly.Server.Network.Polling   (pollingThread)
import           LaunchDarkly.Server.Store             (getAllFlags)
import           LaunchDarkly.Server.Store.Memory      (makeMemoryStoreIO)
import           LaunchDarkly.Server.Evaluate          (evaluateTyped, evaluateDetail)

makeClient :: Config -> IO Client
makeClient config = do
    let runLogger = getField @"logger" config

    status  <- newIORef Uninitialized
    store   <- case getField @"store" config of Nothing -> makeMemoryStoreIO; Just x -> pure x
    manager <- newManager tlsManagerSettings
    events  <- makeEventState config

    let client = Client {..}

    void $ forkIO $ runLogger $ eventThread manager client

    void $ forkIO $ runLogger $ if getField @"streaming" config
        then streamingThread manager client
        else pollingThread   manager client

    pure client

getStatus :: Client -> IO Status
getStatus = readIORef . getField @"status"

allFlags :: Client -> User -> IO (HashMap Text Value)
allFlags client user = if isNothing $ getField @"key" user then pure mempty else do
    flags <- getAllFlags $ getField @"store" client
    evals <- mapM (\flag -> evaluateDetail flag user $ getField @"store" client) flags
    pure $ HM.map (getField @"value" . fst) evals

identify :: Client -> User -> IO ()
identify client user = do
    let user' = userSerializeRedacted (getField @"config" client) user
    x <- makeBaseEvent $ IdentifyEvent { key = getField @"key" user, user = user' }
    queueEvent (getField @"config" client) (getField @"events" client) (EventTypeIdentify x)

track :: Client -> User -> Text -> Maybe Value -> Maybe Double -> IO ()
track client user key value metric = do
    x <- makeBaseEvent $ addUserToEvent (getField @"config" client) user CustomEvent
        { key = key, user = Nothing, userKey = Nothing, metricValue = metric, value = value }
    queueEvent (getField @"config" client) (getField @"events" client) (EventTypeCustom x)

flushEvents :: Client -> IO ()
flushEvents client = putMVar (getField @"flush" $ getField @"events" client) ()

type ValueConverter a = (a -> Value, Value -> Maybe a)

reorderStuff :: ValueConverter a -> Bool -> Client -> Text -> User -> a -> IO (EvaluationDetail a)
reorderStuff converter includeReason client key user fallback = evaluateTyped client key user fallback (fst converter) includeReason (snd converter)

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

boolVariation :: Client -> Text -> User -> Bool -> IO Bool
boolVariation = dropReason . reorderStuff boolConverter False

boolVariationDetail :: Client -> Text -> User -> Bool -> IO (EvaluationDetail Bool)
boolVariationDetail = reorderStuff boolConverter True

stringVariation :: Client -> Text -> User -> Text -> IO Text
stringVariation = dropReason . reorderStuff stringConverter False

stringVariationDetail :: Client -> Text -> User -> Text -> IO (EvaluationDetail Text)
stringVariationDetail = reorderStuff stringConverter True

intVariation :: Client -> Text -> User -> Int -> IO Int
intVariation = dropReason . reorderStuff intConverter False

intVariationDetail :: Client -> Text -> User -> Int -> IO (EvaluationDetail Int)
intVariationDetail = reorderStuff intConverter True

doubleVariation :: Client -> Text -> User -> Double -> IO Double
doubleVariation = dropReason . reorderStuff doubleConverter False

doubleVariationDetail :: Client -> Text -> User -> Double -> IO (EvaluationDetail Double)
doubleVariationDetail = reorderStuff doubleConverter True

jsonVariation :: Client -> Text -> User -> Value -> IO Value
jsonVariation = dropReason . reorderStuff jsonConverter False

jsonVariationDetail :: Client -> Text -> User -> Value -> IO (EvaluationDetail Value)
jsonVariationDetail = reorderStuff jsonConverter True
