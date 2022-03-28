module LaunchDarkly.Server.User.Internal
    ( User(..)
    , mapUser
    , UserI(..)
    , valueOf
    , userSerializeRedacted
    ) where

import           Data.Aeson                          (FromJSON, ToJSON, Value(..), (.:), (.:?), withObject, object, parseJSON, toJSON)
import           Data.Foldable                       (fold)
import           Data.Generics.Product               (getField)
import qualified Data.HashMap.Strict as              HM
import           Data.HashMap.Strict                 (HashMap)
import qualified Data.Set as                         S
import           Data.Set                            (Set)
import           Data.Text                           (Text)
import qualified Data.Vector as                      V
import           GHC.Generics                        (Generic)

import           LaunchDarkly.AesonCompat            (KeyMap, adjustKey, keyToText, deleteKey, filterKeys, insertKey, objectKeys)
import           LaunchDarkly.Server.Config.Internal (ConfigI)

mapUser :: (UserI -> UserI) -> User -> User
mapUser f (User c) = User $ f c

-- | User contains specific attributes of a user of your application
--
-- The only mandatory property is the Key, which must uniquely identify
-- each user. For authenticated users, this may be a username or e-mail address.
-- For anonymous users, this could be an IP address or session ID.
newtype User = User { unwrapUser :: UserI }

data UserI = UserI
    { key                   :: !Text
    , secondary             :: !(Maybe Text)
    , ip                    :: !(Maybe Text)
    , country               :: !(Maybe Text)
    , email                 :: !(Maybe Text)
    , firstName             :: !(Maybe Text)
    , lastName              :: !(Maybe Text)
    , avatar                :: !(Maybe Text)
    , name                  :: !(Maybe Text)
    , anonymous             :: !Bool
    , custom                :: !(HashMap Text Value)
    , privateAttributeNames :: !(Set Text)
    } deriving (Generic)

falseToNothing :: Bool -> Maybe Bool
falseToNothing x = if x then pure x else Nothing

emptyToNothing :: (Eq m, Monoid m) => m -> Maybe m
emptyToNothing x = if x == mempty then mempty else pure x

instance FromJSON UserI where
    parseJSON = withObject "User" $ \o -> UserI
        <$> o .:  "key"
        <*> o .:? "secondary"
        <*> o .:? "ip"
        <*> o .:? "country"
        <*> o .:? "email"
        <*> o .:? "firstName"
        <*> o .:? "lastName"
        <*> o .:? "avatar"
        <*> o .:? "name"
        <*> fmap or (o .:? "anonymous")
        <*> fmap fold (o .:? "custom")
        <*> fmap fold (o .:? "privateAttributeNames")

instance ToJSON UserI where
    toJSON user = object $ filter ((/=) Null . snd)
        [ ("key",                   toJSON $                  getField @"key"                   user)
        , ("secondary",             toJSON $                  getField @"secondary"             user)
        , ("ip",                    toJSON $                  getField @"ip"                    user)
        , ("country",               toJSON $                  getField @"country"               user)
        , ("email",                 toJSON $                  getField @"email"                 user)
        , ("firstName",             toJSON $                  getField @"firstName"             user)
        , ("lastName",              toJSON $                  getField @"lastName"              user)
        , ("avatar",                toJSON $                  getField @"avatar"                user)
        , ("name",                  toJSON $                  getField @"name"                  user)
        , ("anonymous",             toJSON $ falseToNothing $ getField @"anonymous"             user)
        , ("custom",                toJSON $ emptyToNothing $ getField @"custom"                user)
        , ("privateAttributeNames", toJSON $ emptyToNothing $ getField @"privateAttributeNames" user)
        ]

valueOf :: UserI -> Text -> Maybe Value
valueOf user attribute = case attribute of
    "key"       -> pure $ String $ getField @"key" user
    "secondary" -> String <$> getField @"secondary" user
    "ip"        -> String <$> getField @"ip" user
    "country"   -> String <$> getField @"country" user
    "email"     -> String <$> getField @"email" user
    "firstName" -> String <$> getField @"firstName" user
    "lastName"  -> String <$> getField @"lastName" user
    "avatar"    -> String <$> getField @"avatar" user
    "name"      -> String <$> getField @"name" user
    "anonymous" -> pure $ Bool $ getField @"anonymous" user
    x           -> HM.lookup x $ getField @"custom" user

userSerializeRedacted :: ConfigI -> UserI -> Value
userSerializeRedacted config user = if getField @"allAttributesPrivate" config
    then userSerializeAllPrivate user
    else userSerializeRedactedNotAllPrivate (getField @"privateAttributeNames" config) user

fromObject :: Value -> KeyMap Value
fromObject x = case x of (Object o) -> o; _ -> error "expected object"

keysToSet :: KeyMap v -> Set Text
keysToSet = S.fromList . objectKeys

setPrivateAttrs :: Set Text -> KeyMap Value -> Value
setPrivateAttrs private redacted = Object $ insertKey "privateAttrs" (Array $ V.fromList $ map String $ S.toList private) redacted

redact :: Set Text -> KeyMap Value -> KeyMap Value
redact private = filterKeys (\k -> S.notMember (keyToText k) private)

userSerializeAllPrivate :: UserI -> Value
userSerializeAllPrivate user = setPrivateAttrs private (redact private raw) where
    raw     = deleteKey "custom" $ deleteKey "privateAttributeNames" $ fromObject $ toJSON user
    private = S.delete "anonymous" $ S.delete "key" $ S.union (keysToSet raw) (S.fromList $ HM.keys $ getField @"custom" user)

userSerializeRedactedNotAllPrivate :: Set Text -> UserI -> Value
userSerializeRedactedNotAllPrivate globalPrivate user = setPrivateAttrs private redacted where
    raw      = deleteKey "privateAttributeNames" $ fromObject $ toJSON user
    keys     = S.union (keysToSet raw) (keysToSet $ fromObject $ toJSON $ getField @"custom" user)
    private  = S.intersection keys (S.union globalPrivate $ getField @"privateAttributeNames" user)
    redacted = adjustKey (Object . redact private . fromObject) "custom" $ redact private raw
