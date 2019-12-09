module LaunchDarkly.Server.Config.Internal
    ( Config(..)
    , mapConfig
    , ConfigI(..)
    , shouldSendEvents
    ) where

import Control.Monad.Logger      (LoggingT)
import Data.Generics.Product     (getField)
import Data.Text                 (Text)
import Data.Set                  (Set)
import GHC.Natural               (Natural)
import GHC.Generics              (Generic)

import LaunchDarkly.Server.Store (StoreHandle)

mapConfig :: (ConfigI -> ConfigI) -> Config -> Config
mapConfig f (Config c) = Config $ f c

shouldSendEvents :: ConfigI -> Bool
shouldSendEvents config = (not $ getField @"offline" config) && (getField @"sendEvents" config)

-- | Config allows advanced configuration of the LaunchDarkly client.
newtype Config = Config ConfigI

data ConfigI = ConfigI
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
    , sendEvents            :: Bool
    , offline               :: Bool
    , requestTimeoutSeconds :: Natural
    } deriving (Generic)
