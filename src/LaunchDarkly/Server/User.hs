-- | This module is for configuration of the user object.

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

import LaunchDarkly.Server.User.Internal (User(..), mapUser, UserI(..))

-- | Creates a new user identified by the given key.
makeUser :: Text -> User
makeUser key = User $ UserI
    { key                   = key
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

-- | Set the primary key for a user.
userSetKey :: Text -> User -> User
userSetKey = mapUser . setField @"key"

-- | Set the secondary key for a user.
userSetSecondary :: Maybe Text -> User -> User
userSetSecondary = mapUser . setField @"secondary"

-- | Set the IP for a user.
userSetIP :: Maybe Text -> User -> User
userSetIP = mapUser . setField @"ip"

-- | Set the country for a user.
userSetCountry :: Maybe Text -> User -> User
userSetCountry = mapUser . setField @"country"

-- | Set the email for a user.
userSetEmail :: Maybe Text -> User -> User
userSetEmail = mapUser . setField @"email"

-- | Set the first name for a user.
userSetFirstName :: Maybe Text -> User -> User
userSetFirstName = mapUser . setField @"firstName"

-- | Set the last name for a user.
userSetLastName :: Maybe Text -> User -> User
userSetLastName = mapUser . setField @"lastName"

-- | Set the avatar for a user.
userSetAvatar :: Maybe Text -> User -> User
userSetAvatar = mapUser . setField @"avatar"

-- | Set the name for a user.
userSetName :: Maybe Text -> User -> User
userSetName = mapUser . setField @"name"

-- | Set if the user is anonymous or not.
userSetAnonymous :: Bool -> User -> User
userSetAnonymous = mapUser . setField @"anonymous"

-- | Set custom fields for a user.
userSetCustom :: HashMap Text Value -> User -> User
userSetCustom = mapUser . setField @"custom"

-- | This contains list of attributes to keep private, whether they appear at
-- the top-level or Custom The attribute "key" is always sent regardless of
-- whether it is in this list, and "custom" cannot be used to eliminate all
-- custom attributes
userSetPrivateAttributeNames :: Set Text -> User -> User
userSetPrivateAttributeNames = mapUser . setField @"privateAttributeNames"
