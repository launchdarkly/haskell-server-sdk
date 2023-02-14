-- | The public interface for the LaunchDarkly Haskell Redis integration
module LaunchDarkly.Server.Store.Redis.Internal
    ( RedisStoreConfig
    , makeRedisStoreConfig
    , redisConfigSetNamespace
    , makeRedisStore
    , redisUpsertInternal
    ) where

import Control.Exception (throwIO)
import Control.Monad (forM_, void)
import Control.Monad.Catch (Exception, Handler (..), MonadCatch, catches)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Generics.Product (getField, setField)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable (Typeable)
import Database.Redis
    ( Connection
    , ConnectionLostException
    , Redis
    , Reply
    , TxResult (..)
    , del
    , get
    , hget
    , hgetall
    , hset
    , multiExec
    , runRedis
    , set
    , watch
    )
import GHC.Generics (Generic)
import GHC.Natural (Natural)

import LaunchDarkly.AesonCompat (KeyMap, fromList, mapValues, objectKeys, toList)
import LaunchDarkly.Server.Store (PersistentDataStore (..), SerializedItemDescriptor (..), StoreResult (..), byteStringToVersionedData, serializeWithPlaceholder)

-- | Opaque type used to configure the Redis store integration.
data RedisStoreConfig = RedisStoreConfig
    { namespace :: Text
    , connection :: Connection
    }

-- | Create a default config from a given connection pool.
makeRedisStoreConfig :: Connection -> RedisStoreConfig
makeRedisStoreConfig connection =
    RedisStoreConfig
        { namespace = "LaunchDarkly"
        , connection = connection
        }

-- |
-- Configure the Redis key prefix. All keys are prefixed by default before
-- being inserted into Redis. The default prefix is "LaunchDarkly".
redisConfigSetNamespace :: Text -> RedisStoreConfig -> RedisStoreConfig
redisConfigSetNamespace namespace' config = config {namespace = namespace'}

-- |
-- Construct a `PersistentDataStore` that can then be used during SDK
-- configuration.
makeRedisStore :: RedisStoreConfig -> IO PersistentDataStore
makeRedisStore config =
    pure
        PersistentDataStore
            { persistentDataStoreUpsertFeature = redisUpsert config
            , persistentDataStoreGetFeature = redisGetFeature config
            , persistentDataStoreInitialize = redisInitialize config
            , persistentDataStoreIsInitialized = redisIsInitialized config
            , persistentDataStoreAllFeatures = redisGetAll config
            }

data RedisError = RedisError Text deriving (Typeable, Show, Exception)

makeKey :: RedisStoreConfig -> Text -> ByteString
makeKey config key = encodeUtf8 $ T.concat [namespace config, ":", key]

exceptOnReply :: (MonadIO m) => Either Reply a -> m a
exceptOnReply = \case
    Left err -> liftIO $ throwIO $ RedisError $ T.pack $ show err
    Right x -> pure x

run :: RedisStoreConfig -> Redis a -> StoreResult a
run config action =
    catches
        (runRedis (connection config) action >>= pure . pure)
        [ Handler $ \(e :: ConnectionLostException) -> pure $ Left $ T.pack $ show e
        , Handler $ \(RedisError err) -> pure $ Left err
        ]

createSerializedItemDescriptor :: ByteString -> SerializedItemDescriptor
createSerializedItemDescriptor byteString = SerializedItemDescriptor (Just byteString) 0 False

redisInitialize :: RedisStoreConfig -> KeyMap (KeyMap SerializedItemDescriptor) -> StoreResult ()
redisInitialize config values = run config $ do
    del (map (makeKey config) $ objectKeys values) >>= void . exceptOnReply
    forM_ (toList values) $ \(kind, features) -> forM_ (toList features) $ \(key, feature) ->
        (hset (makeKey config kind) (encodeUtf8 key) $ serializeWithPlaceholder feature) >>= void . exceptOnReply
    set (makeKey config "$inited") "" >>= void . exceptOnReply

redisUpsert :: RedisStoreConfig -> Text -> Text -> SerializedItemDescriptor -> StoreResult Bool
redisUpsert = redisUpsertInternal (pure ())

redisUpsertInternal :: IO () -> RedisStoreConfig -> Text -> Text -> SerializedItemDescriptor -> StoreResult Bool
redisUpsertInternal hook config kind key opaque = run config tryUpsert
  where
    tryUpsert =
        watch [space]
            >>= void . exceptOnReply
            >> hget space (encodeUtf8 key)
            >>= exceptOnReply
            >>= \x ->
                (liftIO hook) >> case x of
                    Nothing -> doInsert
                    (Just byteString) -> case byteStringToVersionedData byteString of
                        Nothing -> pure True
                        Just decodedVersion ->
                            if getField @"version" decodedVersion >= getField @"version" opaque
                                then pure False
                                else doInsert
    space = makeKey config kind
    doInsert =
        multiExec (hset space (encodeUtf8 key) (serializeWithPlaceholder opaque)) >>= \case
            TxSuccess _ -> pure True
            TxError err -> liftIO $ throwIO $ RedisError $ T.pack $ show err
            TxAborted -> tryUpsert

redisGetFeature :: RedisStoreConfig -> Text -> Text -> StoreResult (Maybe SerializedItemDescriptor)
redisGetFeature config kind key =
    run config $
        hget (makeKey config kind) (encodeUtf8 key)
            >>= exceptOnReply
            >>= \result -> pure $ (pure . createSerializedItemDescriptor) =<< result

redisIsInitialized :: RedisStoreConfig -> StoreResult Bool
redisIsInitialized config =
    run config $
        get (makeKey config "$inited")
            >>= exceptOnReply
            >>= pure . isJust

redisGetAll :: RedisStoreConfig -> Text -> StoreResult (KeyMap SerializedItemDescriptor)
redisGetAll config kind =
    run config $
        hgetall (makeKey config kind)
            >>= exceptOnReply
            >>= pure . mapValues createSerializedItemDescriptor . fromList . map (\(k, v) -> (decodeUtf8 k, v))
