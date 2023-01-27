module LaunchDarkly.Server.Config.Internal
    ( Config(..)
    , mapConfig
    , unpackConfig
    , ConfigI(..)
    , shouldSendEvents
    ) where

import Control.Monad.Logger               (LoggingT)
import Data.Generics.Product              (getField)
import Data.Text                          (Text)
import Data.Set                           (Set)
import GHC.Natural                        (Natural)
import GHC.Generics                       (Generic)
import Network.HTTP.Client                (Manager)

import LaunchDarkly.Server.Store               (StoreInterface)
import LaunchDarkly.Server.DataSource.Internal (DataSourceFactory)
import LaunchDarkly.Server.Reference (Reference)

mapConfig :: (ConfigI -> ConfigI) -> Config -> Config
mapConfig f (Config c) = Config $ f c

unpackConfig :: Config -> ConfigI
unpackConfig (Config c) = c

shouldSendEvents :: ConfigI -> Bool
shouldSendEvents config = (not $ getField @"offline" config) && (getField @"sendEvents" config)

-- | Config allows advanced configuration of the LaunchDarkly client.
newtype Config = Config ConfigI

data ConfigI = ConfigI
    { key                   :: !Text
    , baseURI               :: !Text
    , streamURI             :: !Text
    , eventsURI             :: !Text
    , storeBackend          :: !(Maybe StoreInterface)
    , storeTTLSeconds       :: !Natural
    , streaming             :: !Bool
    , allAttributesPrivate  :: !Bool
    , privateAttributeNames :: !(Set Reference)
    , flushIntervalSeconds  :: !Natural
    , pollIntervalSeconds   :: !Natural
    , userKeyLRUCapacity    :: !Natural
    , inlineUsersInEvents   :: !Bool
    , eventsCapacity        :: !Natural
    , logger                :: !(LoggingT IO () -> IO ())
    , sendEvents            :: !Bool
    , offline               :: !Bool
    , requestTimeoutSeconds :: !Natural
    , useLdd                :: !Bool
    , dataSourceFactory     :: !(Maybe DataSourceFactory)
    , manager               :: !(Maybe Manager)
    } deriving (Generic)
