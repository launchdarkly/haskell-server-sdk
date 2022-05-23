module Types where

import Data.Function ((&))
import Data.Text (Text)
import qualified LaunchDarkly.Server as LD
import Data.Aeson.Types (Value(..))
import Data.HashMap.Strict (HashMap)
import Data.Aeson (FromJSON, ToJSON, parseJSON, withObject, (.:), (.:?), (.!=))
import GHC.Generics (Generic)
import Data.Set (Set)
import GHC.Natural (Natural)

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
    , inlineUsers :: !(Maybe Bool)
    } deriving (FromJSON, ToJSON, Show, Generic)

data CommandParams = CommandParams
    { command :: !Text
    , evaluate :: !(Maybe EvaluateFlagParams)
    , evaluateAll :: !(Maybe EvaluateAllFlagsParams)
    , customEvent :: !(Maybe CustomEventParams)
    , identifyEvent :: !(Maybe IdentifyEventParams)
    , aliasEvent :: !(Maybe AliasEventParams)
    } deriving (FromJSON, Generic)

data EvaluateFlagParams = EvaluateFlagParams
    { flagKey :: !Text
    , user :: !LD.User
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
    { user :: !LD.User
    , withReasons :: !Bool
    , clientSideOnly :: !Bool
    , detailsOnlyForTrackedFlags :: !Bool
    } deriving (FromJSON, Generic)

data EvaluateAllFlagsResponse = EvaluateAllFlagsResponse
    { state :: !LD.AllFlagsState
    } deriving (ToJSON, Show, Generic)

data CustomEventParams = CustomEventParams
    { eventKey :: !Text
    , user :: !LD.User
    , dataValue :: !(Maybe Value)
    , omitNullData :: !(Maybe Bool)
    , metricValue :: !(Maybe Double)
    } deriving (Generic)

instance FromJSON CustomEventParams where
    parseJSON = withObject "CustomEvent" $ \o -> do
        eventKey <- o .: "eventKey"
        user <- o .: "user"
        dataValue <- o .:? "data"
        omitNullData <- o .:? "omitNullData"
        metricValue <- o .:? "metricValue"
        return $ CustomEventParams { .. }

data IdentifyEventParams = IdentifyEventParams
    { user :: !LD.User
    } deriving (FromJSON, Generic)

data AliasEventParams = AliasEventParams
    { user :: !LD.User
    , previousUser :: !LD.User
    } deriving (FromJSON, Generic)

instance FromJSON LD.User where
    parseJSON = withObject "User" $ \o -> do
        key <- o .: "key"
        secondary <- o .:? "secondary"
        ip <- o .:? "ip"
        country <- o .:? "country"
        email <- o .:? "email"
        firstName <- o .:? "firstName"
        lastName <- o .:? "lastName"
        avatar <- o .:? "avatar"
        name <- o .:? "name"
        anonymous <- o .:? "anonymous" .!= False
        custom <- o .:? "custom" .!= mempty
        privateAttributeNames <- o .:? "privateAttributeNames" .!= mempty
        return $ LD.makeUser key
            & LD.userSetSecondary secondary
            & LD.userSetIP ip
            & LD.userSetCountry country
            & LD.userSetEmail email
            & LD.userSetFirstName firstName
            & LD.userSetLastName lastName
            & LD.userSetAvatar avatar
            & LD.userSetName name
            & LD.userSetAnonymous anonymous
            & LD.userSetCustom custom
            & LD.userSetPrivateAttributeNames privateAttributeNames
