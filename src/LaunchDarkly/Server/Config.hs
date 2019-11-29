module LaunchDarkly.Server.Config
    ( Config(..)
    , makeConfig
    ) where

import Control.Monad.Logger      (LoggingT, runStdoutLoggingT)
import Data.Text                 (Text)
import Data.Set                  (Set)
import Data.Monoid               (mempty)
import GHC.Natural               (Natural)
import GHC.Generics              (Generic)

import LaunchDarkly.Server.Store (StoreHandle)

data Config = Config
    { key                   :: Text
    , baseURI               :: Text
    , streamURI             :: Text
    , eventsURI             :: Text
    , store                 :: Maybe (StoreHandle IO)
    , streaming             :: Bool
    , allAttributesPrivate  :: Bool
    , privateAttributeNames :: Set Text
    , flushIntervalSeconds  :: Natural
    , pollIntervalSeconds   :: Natural
    , userKeyLRUCapacity    :: Natural
    , inlineUsersInEvents   :: Bool
    , eventsCapacity        :: Natural
    , logger                :: LoggingT IO () -> IO ()
    } deriving (Generic)

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
