module LaunchDarkly.Server.Config.Internal
    ( Config(..)
    , mapConfig
    , ConfigI(..)
    , shouldSendEvents
    , ApplicationInfo
    , makeApplicationInfo
    , withApplicationValue
    , getApplicationInfoHeader
    ) where

import Control.Monad.Logger               (LoggingT)
import Data.Generics.Product              (getField)
import Data.Text                          (Text)
import qualified Data.Text as T
import Data.Set                           (Set)
import GHC.Natural                        (Natural)
import GHC.Generics                       (Generic)
import Network.HTTP.Client                (Manager)

import LaunchDarkly.Server.Store               (StoreInterface)
import LaunchDarkly.Server.DataSource.Internal (DataSourceFactory)
import LaunchDarkly.AesonCompat (KeyMap, insertKey, emptyObject, toList)
import qualified LaunchDarkly.AesonCompat as AesonCompat
import Data.List (sortBy)
import Control.Lens ((&))
import Data.Ord (comparing)

mapConfig :: (ConfigI -> ConfigI) -> Config -> Config
mapConfig f (Config c) = Config $ f c

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
    , privateAttributeNames :: !(Set Text)
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
    , applicationInfo       :: !(Maybe ApplicationInfo)
    } deriving (Generic)

-- | An object that allows configuration of application metadata.
--
-- Application metadata may be used in LaunchDarkly analytics or other product
-- features, but does not affect feature flag evaluations.
--
-- To use these properties, provide an instance of ApplicationInfo to the 'Config' with 'configSetApplicationInfo'.
newtype ApplicationInfo = ApplicationInfo (KeyMap Text) deriving (Show, Eq)

-- | Create a default instance
makeApplicationInfo :: ApplicationInfo
makeApplicationInfo = ApplicationInfo emptyObject

-- | Set a new name / value pair into the application info instance.
--
-- Values have the following restrictions:
-- - Cannot be empty
-- - Cannot exceed 64 characters in length
-- - Can only contain a-z, A-Z, 0-9, period (.), dash (-), and underscore (_).
--
-- Invalid values or unsupported keys will be ignored.
withApplicationValue :: Text -> Text -> ApplicationInfo -> ApplicationInfo
withApplicationValue _ "" info = info
withApplicationValue name value info@(ApplicationInfo map)
    | (name `elem` ["id", "version"]) == False = info
    | T.length(value) > 64 = info
    | (all (`elem` ['a'..'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['.', '-', '_']) (T.unpack value)) == False = info
    | otherwise = ApplicationInfo $ insertKey name value map

getApplicationInfoHeader :: ApplicationInfo -> Maybe Text
getApplicationInfoHeader (ApplicationInfo values)
    | AesonCompat.null values = Nothing
    | otherwise = toList values
        & sortBy (comparing fst)
        & map makeTag
        & T.unwords
        & Just
    where makeTag (key, value) = "application-" <> key <> "/" <> value
