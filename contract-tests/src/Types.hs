module Types where

import Data.Function ((&))
import Data.Text (Text)
import qualified LaunchDarkly.Server as LD
import Data.Aeson.Types (Value(..))
import Data.HashMap.Strict (HashMap)
import Data.Aeson (FromJSON, ToJSON, toJSON, parseJSON, object, withObject, (.:), (.:?), (.!=))
import GHC.Generics (Generic)
import Data.Set (Set)
import GHC.Natural (Natural)
import Data.Maybe (fromMaybe)

data CreateClientParams = CreateClientParams
    { tag :: !Text
    , configuration :: !ConfigurationParams
    } deriving (FromJSON, ToJSON, Show, Generic)

data ConfigurationParams = ConfigurationParams
    { credential :: !Text
    , startWaitTimeMs :: !(Maybe Int)
    , initCanFail :: !(Maybe Bool)
    , streaming :: !(Maybe StreamingParams)
    , events :: !(Maybe EventParams)
    , tags :: !(Maybe TagParams)
    } deriving (FromJSON, ToJSON, Show, Generic)

data StreamingParams = StreamingParams
    { baseUri :: !(Maybe Text)
    , initialRetryDelayMs :: !(Maybe Int)
    } deriving (FromJSON, ToJSON, Show, Generic)

data EventParams = EventParams
    { baseUri :: !(Maybe Text)
    , capacity :: !(Maybe Natural)
    , enableDiagnostics :: !(Maybe Bool)
    , allAttributesPrivate :: !(Maybe Bool)
    , globalPrivateAttributes :: !(Maybe (Set Text))
    , flushIntervalMs :: !(Maybe Natural)
    } deriving (FromJSON, ToJSON, Show, Generic)

data TagParams = TagParams
    { applicationId :: !(Maybe Text)
    , applicationVersion :: !(Maybe Text)
    } deriving (FromJSON, ToJSON, Show, Generic)

data CommandParams = CommandParams
    { command :: !Text
    , evaluate :: !(Maybe EvaluateFlagParams)
    , evaluateAll :: !(Maybe EvaluateAllFlagsParams)
    , customEvent :: !(Maybe CustomEventParams)
    , identifyEvent :: !(Maybe IdentifyEventParams)
    , contextBuild :: !(Maybe ContextBuildParams)
    , contextConvert :: !(Maybe ContextConvertParams)
    , secureModeHash :: !(Maybe SecureModeHashParams)
    } deriving (FromJSON, Generic)

data EvaluateFlagParams = EvaluateFlagParams
    { flagKey :: !Text
    , context :: !LD.Context
    , valueType :: !Text
    , defaultValue :: !Value
    , detail :: !Bool
    } deriving (FromJSON, Generic)

data EvaluateFlagResponse = EvaluateFlagResponse
    { value :: !Value
    , variationIndex :: !(Maybe Integer)
    , reason :: !(Maybe LD.EvaluationReason)
    } deriving (ToJSON, Show, Generic)

data EvaluateAllFlagsParams = EvaluateAllFlagsParams
    { context :: !LD.Context
    , withReasons :: !Bool
    , clientSideOnly :: !Bool
    , detailsOnlyForTrackedFlags :: !Bool
    } deriving (FromJSON, Generic)

data EvaluateAllFlagsResponse = EvaluateAllFlagsResponse
    { state :: !LD.AllFlagsState
    } deriving (ToJSON, Show, Generic)

data CustomEventParams = CustomEventParams
    { eventKey :: !Text
    , context :: !LD.Context
    , dataValue :: !(Maybe Value)
    , omitNullData :: !(Maybe Bool)
    , metricValue :: !(Maybe Double)
    } deriving (Generic)

instance FromJSON CustomEventParams where
    parseJSON = withObject "CustomEvent" $ \o -> do
        eventKey <- o .: "eventKey"
        context <- o .: "context"
        dataValue <- o .:? "data"
        omitNullData <- o .:? "omitNullData"
        metricValue <- o .:? "metricValue"
        return $ CustomEventParams { .. }

data IdentifyEventParams = IdentifyEventParams
    { context :: !LD.Context
    } deriving (FromJSON, Generic)

data ContextBuildParams = ContextBuildParams
    { single :: !(Maybe ContextBuildParam)
    , multi :: !(Maybe [ContextBuildParam])
    } deriving (FromJSON, Generic)

data ContextBuildParam = ContextBuildParam
    { kind :: !(Maybe Text)
    , key :: !Text
    , name :: !(Maybe Text)
    , anonymous :: !(Maybe Bool)
    , private :: !(Maybe (Set Text))
    , custom :: !(Maybe (HashMap Text Value))
    } deriving (FromJSON, Generic)

data ContextConvertParams = ContextConvertParams
    { input :: !Text
    } deriving (FromJSON, Generic)

data ContextResponse = ContextResponse
    { output :: !(Maybe Text)
    , errorMessage :: !(Maybe Text)
    } deriving (Generic)

instance ToJSON ContextResponse where
    toJSON (ContextResponse { output = Just o, errorMessage = Nothing }) = object [ ("output", String o) ]
    toJSON (ContextResponse { output = _, errorMessage = Just e }) = object [ ("error", String e) ]
    toJSON _ = object [ ("error", String "Invalid context response was generated") ]

data SecureModeHashParams = SecureModeHashParams
    { context :: !(Maybe LD.Context)
    } deriving (FromJSON, Generic)

data SecureModeHashResponse = SecureModeHashResponse
    { result :: !Text
    } deriving (ToJSON, Show, Generic)
