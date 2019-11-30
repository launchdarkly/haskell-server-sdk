module LaunchDarkly.Server.Config
    ( Config
    , makeConfig
    , configSetKey
    , configSetBaseURI
    , configSetStreamURI
    , configSetEventsURI
    , configSetStore
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

configSetKey :: Text -> Config -> Config
configSetKey = setField @"key"

configSetBaseURI :: Text -> Config -> Config
configSetBaseURI = setField @"baseURI"

configSetStreamURI :: Text -> Config -> Config
configSetStreamURI = setField @"streamURI"

configSetEventsURI :: Text -> Config -> Config
configSetEventsURI = setField @"eventsURI"

configSetStore :: Maybe (StoreHandle IO) -> Config -> Config
configSetStore = setField @"store"

configSetStreaming :: Bool -> Config -> Config
configSetStreaming = setField @"streaming"

configSetAllAttributesPrivate :: Bool -> Config -> Config
configSetAllAttributesPrivate = setField @"allAttributesPrivate"

configSetPrivateAttributeNames :: Set Text -> Config -> Config
configSetPrivateAttributeNames = setField @"privateAttributeNames"

configSetFlushIntervalSeconds :: Natural -> Config -> Config
configSetFlushIntervalSeconds = setField @"flushIntervalSeconds"

configSetPollIntervalSeconds :: Natural -> Config -> Config
configSetPollIntervalSeconds = setField @"pollIntervalSeconds"

configSetUserKeyLRUCapacity :: Natural -> Config -> Config
configSetUserKeyLRUCapacity = setField @"userKeyLRUCapacity"

configSetInlineUsersInEvents :: Bool -> Config -> Config
configSetInlineUsersInEvents = setField @"inlineUsersInEvents"

configSetEventsCapacity :: Natural -> Config -> Config
configSetEventsCapacity = setField @"eventsCapacity"

configSetLogger :: (LoggingT IO () -> IO ()) -> Config -> Config
configSetLogger = setField @"logger"
