module LaunchDarkly.Server.User
    ( User
    , makeUser
    , userSetKey
    , userSetSecondary
    , userSetIP
    , userSetCountry
    , userSetEmail
    , userSetFirstName
    , userSetLastName
    , userSetAvatar
    , userSetName
    , userSetAnonymous
    , userSetCustom
    , userSetPrivateAttributeNames
    ) where

import Data.Aeson                        (Value)
import Data.Generics.Product             (setField)
import Data.HashMap.Strict               (HashMap)
import Data.Set                          (Set)
import Data.Text                         (Text)

import LaunchDarkly.Server.User.Internal (User(..))

makeUser :: Text -> User
makeUser key = User
    { key                   = pure key
    , secondary             = mempty
    , ip                    = mempty
    , country               = mempty
    , email                 = mempty
    , firstName             = mempty
    , lastName              = mempty
    , avatar                = mempty
    , name                  = mempty
    , anonymous             = False
    , custom                = mempty
    , privateAttributeNames = mempty
    }

userSetKey :: Text -> User -> User
userSetKey key = setField @"key" (pure key)

userSetSecondary :: Maybe Text -> User -> User
userSetSecondary = setField @"secondary"

userSetIP :: Maybe Text -> User -> User
userSetIP = setField @"ip"

userSetCountry :: Maybe Text -> User -> User
userSetCountry = setField @"country"

userSetEmail :: Maybe Text -> User -> User
userSetEmail = setField @"email"

userSetFirstName :: Maybe Text -> User -> User
userSetFirstName = setField @"firstName"

userSetLastName :: Maybe Text -> User -> User
userSetLastName = setField @"lastName"

userSetAvatar :: Maybe Text -> User -> User
userSetAvatar = setField @"avatar"

userSetName :: Maybe Text -> User -> User
userSetName = setField @"name"

userSetAnonymous :: Bool -> User -> User
userSetAnonymous = setField @"anonymous"

userSetCustom :: HashMap Text Value -> User -> User
userSetCustom = setField @"custom"

userSetPrivateAttributeNames :: Set Text -> User -> User
userSetPrivateAttributeNames = setField @"privateAttributeNames"
