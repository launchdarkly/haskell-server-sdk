-- | The public interface for the LaunchDarkly Haskell Redis integration

module LaunchDarkly.Server.Store.Redis.Internal
    ( RedisStoreConfig
    , makeRedisStoreConfig
    , redisConfigSetNamespace
    , makeRedisStore
    , redisUpsertInternal
    ) where

import           Data.Maybe                 (isJust)
import           Control.Monad              (forM_, void)
import           Control.Monad.Catch        (MonadCatch, Exception, catches, Handler(..))
import           Control.Exception          (throwIO)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Aeson                 (FromJSON, ToJSON, decode, encode)
import           Data.ByteString            (ByteString)
import           Data.ByteString.Lazy       (toStrict, fromStrict)
import           Data.Text                  (Text)
import qualified Data.Text as               T
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Data.Typeable              (Typeable)
import qualified Data.HashMap.Strict as     HM
import           Data.HashMap.Strict        (HashMap)
import           Data.Generics.Product      (getField, setField)
import           Database.Redis             (ConnectionLostException, Reply, multiExec, runRedis, del, get
                                            , set, hget, hgetall, hset, watch, Redis, Connection, TxResult(..))
import           GHC.Natural                (Natural)
import           GHC.Generics               (Generic)

import           LaunchDarkly.Server.Store  (StoreInterface(..), RawFeature(..), StoreResult(..))

data MinimalFeature = MinimalFeature
    { key     :: Text
    , version :: Natural
    , deleted :: Bool
    } deriving (Generic, ToJSON, FromJSON)

-- | Opaque type used to configure the Redis store integration.
data RedisStoreConfig = RedisStoreConfig
    { namespace  :: Text
    , connection :: Connection
    }

-- | Create a default config from a given connection pool.
makeRedisStoreConfig :: Connection -> RedisStoreConfig
makeRedisStoreConfig connection = RedisStoreConfig
    { namespace  = "LaunchDarkly"
    , connection = connection
    }

-- | Configure the Redis key prefix. All keys are prefixed by default before
-- being inserted into Redis. The default prefix is "LaunchDarkly".
redisConfigSetNamespace :: Text -> RedisStoreConfig -> RedisStoreConfig
redisConfigSetNamespace namespace' config = config { namespace = namespace' }

-- | Construct a `StoreInterface` that can then be used during SDK
-- configuration.
makeRedisStore :: RedisStoreConfig -> IO StoreInterface
makeRedisStore config = pure StoreInterface
    { storeInterfaceUpsertFeature = redisUpsert        config
    , storeInterfaceGetFeature    = redisGetFeature    config
    , storeInterfaceInitialize    = redisInitialize    config
    , storeInterfaceIsInitialized = redisIsInitialized config
    , storeInterfaceAllFeatures   = redisGetAll        config
    }

data RedisError = RedisError Text deriving (Typeable, Show, Exception)

makeKey :: RedisStoreConfig -> Text -> ByteString
makeKey config key = encodeUtf8 $ T.concat [namespace config, ":", key]

exceptOnReply :: (MonadIO m) => Either Reply a -> m a
exceptOnReply =  \case
    Left err -> liftIO $ throwIO $ RedisError $ T.pack $ show err
    Right x  -> pure x

run :: RedisStoreConfig -> Redis a -> StoreResult a
run config action = catches (runRedis (connection config) action >>= pure . pure)
    [ Handler $ \(e :: ConnectionLostException) -> pure $ Left $ T.pack $ show e
    , Handler $ \(RedisError err) -> pure $ Left err
    ]

decodeMinimal :: ByteString -> Maybe MinimalFeature
decodeMinimal = decode . fromStrict

rawToOpaque :: ByteString -> RawFeature
rawToOpaque raw = case decodeMinimal raw of
    Nothing      -> RawFeature Nothing 0
    Just decoded -> RawFeature (if getField @"deleted" decoded then Nothing else pure raw)
        (getField @"version" decoded)

opaqueToRep :: Text -> RawFeature -> ByteString
opaqueToRep key opaque = case rawFeatureBuffer opaque of
    Just buffer -> buffer
    Nothing     -> toStrict $ encode $ MinimalFeature key (rawFeatureVersion opaque) True

redisInitialize :: RedisStoreConfig -> HashMap Text (HashMap Text RawFeature) -> StoreResult ()
redisInitialize config values = run config $ do
    del (map (makeKey config) $ HM.keys values) >>= void . exceptOnReply
    forM_ (HM.toList values) $ \(kind, features) -> forM_ (HM.toList features) $ \(key, feature) ->
        (hset (makeKey config kind) (encodeUtf8 key) $ opaqueToRep key feature) >>= void . exceptOnReply
    set (makeKey config "$inited") "" >>= void . exceptOnReply

redisUpsert :: RedisStoreConfig -> Text -> Text -> RawFeature -> StoreResult Bool
redisUpsert = redisUpsertInternal (pure ())

redisUpsertInternal :: IO () -> RedisStoreConfig -> Text -> Text -> RawFeature -> StoreResult Bool
redisUpsertInternal hook config kind key opaque = run config tryUpsert where
    tryUpsert = watch [space] >>= void . exceptOnReply >>
        hget space (encodeUtf8 key) >>= exceptOnReply >>= \x -> (liftIO hook) >> case x of
            Nothing    -> doInsert
            (Just raw) -> case decodeMinimal raw of
                Nothing      -> pure True
                Just decoded -> if getField @"version" decoded >= rawFeatureVersion opaque
                    then pure False else doInsert
    space     = makeKey config kind
    doInsert  = multiExec (hset space (encodeUtf8 key) (opaqueToRep key opaque)) >>= \case
        TxSuccess _ -> pure True
        TxError err -> liftIO $ throwIO $ RedisError $ T.pack $ show err
        TxAborted   -> tryUpsert

redisGetFeature :: RedisStoreConfig -> Text -> Text -> StoreResult RawFeature
redisGetFeature config kind key = run config $ hget (makeKey config kind) (encodeUtf8 key)
    >>= exceptOnReply >>= \case
        Nothing    -> pure $ RawFeature Nothing 0
        (Just raw) -> pure $ rawToOpaque raw

redisIsInitialized :: RedisStoreConfig -> StoreResult Bool
redisIsInitialized config = run config $ get (makeKey config "$inited")
    >>= exceptOnReply >>= pure . isJust

redisGetAll :: RedisStoreConfig -> Text -> StoreResult (HashMap Text RawFeature)
redisGetAll config kind = run config $ hgetall (makeKey config kind)
    >>= exceptOnReply >>= pure . HM.map rawToOpaque . HM.fromList . map (\(k, v) -> (decodeUtf8 k, v))
