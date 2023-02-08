{-# LANGUAGE NumericUnderscores #-}

-- | This module is for configuration of the SDK.
module LaunchDarkly.Server.Config
    ( Config
    , makeConfig
    , configSetKey
    , configSetBaseURI
    , configSetStreamURI
    , configSetEventsURI
    , configSetStreaming
    , configSetInitialRetryDelay
    , configSetAllAttributesPrivate
    , configSetPrivateAttributeNames
    , configSetFlushIntervalSeconds
    , configSetPollIntervalSeconds
    , configSetContextKeyLRUCapacity
    , configSetUserKeyLRUCapacity
    , configSetEventsCapacity
    , configSetLogger
    , configSetManager
    , configSetSendEvents
    , configSetOffline
    , configSetRequestTimeoutSeconds
    , configSetStoreBackend
    , configSetStoreTTL
    , configSetUseLdd
    , configSetDataSourceFactory
    , configSetApplicationInfo
    , ApplicationInfo
    , makeApplicationInfo
    , withApplicationValue
    ) where

import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Data.Generics.Product (setField)
import Data.Set (Set)
import Data.Text (Text, dropWhileEnd)
import GHC.Natural (Natural)
import Network.HTTP.Client (Manager)

import LaunchDarkly.Server.Config.Internal (ApplicationInfo, Config (..), makeApplicationInfo, withApplicationValue)
import LaunchDarkly.Server.DataSource.Internal (DataSourceFactory)
import LaunchDarkly.Server.Reference (Reference)
import LaunchDarkly.Server.Store (StoreInterface)

-- | Create a default configuration from a given SDK key.
makeConfig :: Text -> Config
makeConfig key =
    Config
        { key = key
        , baseURI = "https://app.launchdarkly.com"
        , streamURI = "https://stream.launchdarkly.com"
        , eventsURI = "https://events.launchdarkly.com"
        , storeBackend = Nothing
        , storeTTLSeconds = 10
        , streaming = True
        , initialRetryDelay = 1_000
        , allAttributesPrivate = False
        , privateAttributeNames = mempty
        , flushIntervalSeconds = 5
        , pollIntervalSeconds = 30
        , contextKeyLRUCapacity = 1_000
        , eventsCapacity = 10_000
        , logger = runStdoutLoggingT
        , sendEvents = True
        , offline = False
        , requestTimeoutSeconds = 30
        , useLdd = False
        , dataSourceFactory = Nothing
        , manager = Nothing
        , applicationInfo = Nothing
        }

-- | Set the SDK key used to authenticate with LaunchDarkly.
configSetKey :: Text -> Config -> Config
configSetKey = setField @"key"

-- |
-- The base URI of the main LaunchDarkly service. This should not normally be
-- changed except for testing.
configSetBaseURI :: Text -> Config -> Config
configSetBaseURI = setField @"baseURI" . dropWhileEnd ((==) '/')

-- |
-- The base URI of the LaunchDarkly streaming service. This should not
-- normally be changed except for testing.
configSetStreamURI :: Text -> Config -> Config
configSetStreamURI = setField @"streamURI" . dropWhileEnd ((==) '/')

-- |
-- The base URI of the LaunchDarkly service that accepts analytics events.
-- This should not normally be changed except for testing.
configSetEventsURI :: Text -> Config -> Config
configSetEventsURI = setField @"eventsURI" . dropWhileEnd ((==) '/')

-- | Configures a handle to an external store such as Redis.
configSetStoreBackend :: Maybe StoreInterface -> Config -> Config
configSetStoreBackend = setField @"storeBackend"

-- |
-- When a store backend is configured, control how long values should be
-- cached in memory before going back to the backend.
configSetStoreTTL :: Natural -> Config -> Config
configSetStoreTTL = setField @"storeTTLSeconds"

-- |
-- Sets whether streaming mode should be enabled. By default, streaming is
-- enabled. It should only be disabled on the advice of LaunchDarkly support.
configSetStreaming :: Bool -> Config -> Config
configSetStreaming = setField @"streaming"

-- |
-- The initial delay in milliseconds before reconnecting after an error in the
-- SSE client. Defaults to 1 second.
--
-- This only applies to the streaming connection. Providing a non-positive
-- integer is a no-op.
configSetInitialRetryDelay :: Int -> Config -> Config
configSetInitialRetryDelay seconds config
    | seconds <= 0 = config
    | otherwise = setField @"initialRetryDelay" seconds config

-- |
-- Sets whether or not all context attributes (other than the key) should be
-- hidden from LaunchDarkly. If this is true, all context attribute values will
-- be private, not just the attributes specified in PrivateAttributeNames.
configSetAllAttributesPrivate :: Bool -> Config -> Config
configSetAllAttributesPrivate = setField @"allAttributesPrivate"

-- |
-- Marks a set of context attribute names private. Any contexts sent to
-- LaunchDarkly with this configuration active will have attributes with these
-- names removed.
configSetPrivateAttributeNames :: Set Reference -> Config -> Config
configSetPrivateAttributeNames = setField @"privateAttributeNames"

-- |
-- The time between flushes of the event buffer. Decreasing the flush
-- interval means that the event buffer is less likely to reach capacity.
configSetFlushIntervalSeconds :: Natural -> Config -> Config
configSetFlushIntervalSeconds = setField @"flushIntervalSeconds"

-- | The polling interval (when streaming is disabled).
configSetPollIntervalSeconds :: Natural -> Config -> Config
configSetPollIntervalSeconds = setField @"pollIntervalSeconds"

-- |
-- The number of context keys that the event processor can remember at any
-- one time, so that duplicate context details will not be sent in analytics
-- events.
configSetContextKeyLRUCapacity :: Natural -> Config -> Config
configSetContextKeyLRUCapacity = setField @"contextKeyLRUCapacity"

{-# DEPRECATED configSetUserKeyLRUCapacity "Use configSetContextKeyLRUCapacity instead" #-}

-- |
-- Deprecated historically named function which proxies to
-- 'configSetContextKeyLRUCapacity'.
configSetUserKeyLRUCapacity :: Natural -> Config -> Config
configSetUserKeyLRUCapacity = configSetContextKeyLRUCapacity

-- |
-- The capacity of the events buffer. The client buffers up to this many
-- events in memory before flushing. If the capacity is exceeded before the
-- buffer is flushed, events will be discarded.
configSetEventsCapacity :: Natural -> Config -> Config
configSetEventsCapacity = setField @"eventsCapacity"

-- | Set the logger to be used by the client.
configSetLogger :: (LoggingT IO () -> IO ()) -> Config -> Config
configSetLogger = setField @"logger"

-- |
-- Sets whether to send analytics events back to LaunchDarkly. By default,
-- the client will send events. This differs from Offline in that it only
-- affects sending events, not streaming or polling for events from the server.
configSetSendEvents :: Bool -> Config -> Config
configSetSendEvents = setField @"sendEvents"

-- |
-- Sets whether this client is offline. An offline client will not make any
-- network connections to LaunchDarkly, and will return default values for all
-- feature flags.
configSetOffline :: Bool -> Config -> Config
configSetOffline = setField @"offline"

-- |
-- Sets how long an the HTTP client should wait before a response is
-- returned.
configSetRequestTimeoutSeconds :: Natural -> Config -> Config
configSetRequestTimeoutSeconds = setField @"requestTimeoutSeconds"

-- |
-- Sets whether this client should use the LaunchDarkly Relay Proxy in daemon
-- mode. In this mode, the client does not subscribe to the streaming or
-- polling API, but reads data only from the feature store. See:
-- https://docs.launchdarkly.com/home/relay-proxy
configSetUseLdd :: Bool -> Config -> Config
configSetUseLdd = setField @"useLdd"

-- |
-- Sets a data source to use instead of the default network based data source
-- see "LaunchDarkly.Server.Integrations.FileData"
configSetDataSourceFactory :: Maybe DataSourceFactory -> Config -> Config
configSetDataSourceFactory = setField @"dataSourceFactory"

-- |
-- Sets the 'Manager' to use with the client. If not set explicitly a new
-- 'Manager' will be created when creating the client.
configSetManager :: Manager -> Config -> Config
configSetManager = setField @"manager" . Just

-- |
-- An object that allows configuration of application metadata.
--
-- Application metadata may be used in LaunchDarkly analytics or other product
-- features, but does not affect feature flag evaluations.
--
-- If you want to set non-default values for any of these fields, provide the
-- appropriately configured dict to the 'Config' object.
configSetApplicationInfo :: ApplicationInfo -> Config -> Config
configSetApplicationInfo = setField @"applicationInfo" . Just
