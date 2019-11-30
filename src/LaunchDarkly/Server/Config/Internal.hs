module LaunchDarkly.Server.Config.Internal (Config(..)) where

import Control.Monad.Logger      (LoggingT)
import Data.Text                 (Text)
import Data.Set                  (Set)
import GHC.Natural               (Natural)
import GHC.Generics              (Generic)

import LaunchDarkly.Server.Store (StoreHandle)

-- | Config allows advanced configuration of the LaunchDarkly client.
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
