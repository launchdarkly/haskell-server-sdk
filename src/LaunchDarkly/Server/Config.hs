-- | This module is for configuration of the SDK.

module LaunchDarkly.Server.Config
    ( Config
    , makeConfig
    , configSetKey
    , configSetBaseURI
    , configSetStreamURI
    , configSetEventsURI
    , configSetStreaming
    , configSetAllAttributesPrivate
    , configSetPrivateAttributeNames
    , configSetFlushIntervalSeconds
    , configSetPollIntervalSeconds
    , configSetUserKeyLRUCapacity
    , configSetInlineUsersInEvents
    , configSetEventsCapacity
    , configSetLogger
    ) where

import Control.Monad.Logger                (LoggingT, runStdoutLoggingT)
import Data.Generics.Product               (setField)
import Data.Set                            (Set)
import Data.Text                           (Text)
import Data.Monoid                         (mempty)
import GHC.Natural                         (Natural)

import LaunchDarkly.Server.Config.Internal (Config(..))
import LaunchDarkly.Server.Store           (StoreHandle)

-- | Create a default configuration from a given SDK key.
makeConfig :: Text -> Config
makeConfig key = Config
    { key                   = key
    , baseURI               = "https://app.launchdarkly.com"
    , streamURI             = "https://stream.launchdarkly.com"
    , eventsURI             = "https://events.launchdarkly.com"
    , store                 = Nothing
    , streaming             = True
    , allAttributesPrivate  = False
    , privateAttributeNames = mempty
    , flushIntervalSeconds  = 5
    , pollIntervalSeconds   = 30
    , userKeyLRUCapacity    = 1000
    , inlineUsersInEvents   = False
    , eventsCapacity        = 10000
    , logger                = runStdoutLoggingT
    }

-- | Set the SDK key used to authenticate with LaunchDarkly.
configSetKey :: Text -> Config -> Config
configSetKey = setField @"key"

-- | The base URI of the main LaunchDarkly service. This should not normally be
-- changed except for testing.
configSetBaseURI :: Text -> Config -> Config
configSetBaseURI = setField @"baseURI"

-- | The base URI of the LaunchDarkly streaming service. This should not
-- normally be changed except for testing.
configSetStreamURI :: Text -> Config -> Config
configSetStreamURI = setField @"streamURI"

-- | The base URI of the LaunchDarkly service that accepts analytics events.
-- This should not normally be changed except for testing.
configSetEventsURI :: Text -> Config -> Config
configSetEventsURI = setField @"eventsURI"

configSetStore :: Maybe (StoreHandle IO) -> Config -> Config
configSetStore = setField @"store"

-- | Sets whether streaming mode should be enabled. By default, streaming is
-- enabled. It should only be disabled on the advice of LaunchDarkly support.
configSetStreaming :: Bool -> Config -> Config
configSetStreaming = setField @"streaming"

-- | Sets whether or not all user attributes (other than the key) should be
-- hidden from LaunchDarkly. If this is true, all user attribute values will be
-- private, not just the attributes specified in PrivateAttributeNames.
configSetAllAttributesPrivate :: Bool -> Config -> Config
configSetAllAttributesPrivate = setField @"allAttributesPrivate"

-- | Marks a set of user attribute names private. Any users sent to LaunchDarkly
-- with this configuration active will have attributes with these names removed.
configSetPrivateAttributeNames :: Set Text -> Config -> Config
configSetPrivateAttributeNames = setField @"privateAttributeNames"

-- | The time between flushes of the event buffer. Decreasing the flush interval
-- means that the event buffer is less likely to reach capacity.
configSetFlushIntervalSeconds :: Natural -> Config -> Config
configSetFlushIntervalSeconds = setField @"flushIntervalSeconds"

-- | The polling interval (when streaming is disabled).
configSetPollIntervalSeconds :: Natural -> Config -> Config
configSetPollIntervalSeconds = setField @"pollIntervalSeconds"

-- | The number of user keys that the event processor can remember at any one
-- time, so that duplicate user details will not be sent in analytics events.
configSetUserKeyLRUCapacity :: Natural -> Config -> Config
configSetUserKeyLRUCapacity = setField @"userKeyLRUCapacity"

-- | Set to true if you need to see the full user details in every analytics
-- event.
configSetInlineUsersInEvents :: Bool -> Config -> Config
configSetInlineUsersInEvents = setField @"inlineUsersInEvents"

-- | The capacity of the events buffer. The client buffers up to this many
-- events in memory before flushing. If the capacity is exceeded before the
-- buffer is flushed, events will be discarded.
configSetEventsCapacity :: Natural -> Config -> Config
configSetEventsCapacity = setField @"eventsCapacity"

-- | Set the logger to be used by the client.
configSetLogger :: (LoggingT IO () -> IO ()) -> Config -> Config
configSetLogger = setField @"logger"
