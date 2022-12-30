module LaunchDarkly.Server.Context.Internal
  ( Context(..)
  , ContextI(..)
  , SingleContext(..)
  , MultiContext(..)
  , canonicalizeKey
  , mapContext
  , mapValue
  , contextSetName
  , contextSetAnonymous
  , contextSetAttribute
  , contextSetPrivateAttributes
  , contextIsValid
  , contextError
  , unwrapSingleContext
  , contextKind
  , contextKey
  )
where

import Data.Aeson (Value(..))
import Data.Generics.Product (getField, setField)
import Data.Text (Text, replace)
import GHC.Generics (Generic)
import Data.Function ((&))
import LaunchDarkly.AesonCompat (KeyMap, singleton, insertKey)

mapContext :: (ContextI -> ContextI) -> Context -> Context
mapContext f (Context c) = Context $ f c

mapValue :: (ContextI -> a) -> Context -> a
mapValue f (Context c) = f c

-- | Context is a collection of attributes that can be referenced in flag
-- evaluations and analytics events.
--
-- To create a Context of a single kind, such as a user, you may use
-- 'LaunchDarkly.Server.Context.makeContext'.
--
-- To create an LDContext with multiple kinds, use 'LaunchDarkly.Server.Context.makeMultiContext'.
--
-- Additional properties can be set on a single-kind context using the set methods found in
-- "LaunchDarkly.Server.Context".
--
-- Each method will always return a Context. However, that
-- Context may be invalid. You can check the validity of the resulting
-- context, and the associated errors by calling 'LaunchDarkly.Server.Context.contextIsValid' and
-- 'LaunchDarkly.Server.Context.contextError'.
newtype Context = Context { unwrapContext :: ContextI } deriving (Show, Eq)

data ContextI =
  Single SingleContext
  | Multi MultiContext
  | InvalidContext { error :: !Text } deriving (Show, Eq)

data SingleContext = SingleContext
  { key :: !Text
  , fullKey :: !Text
  , kind ::  !Text
  , name :: !(Maybe Text)
  , anonymous :: !Bool
  , attributes :: !(Maybe (KeyMap Value))
  , privateAttributes :: !(Maybe [Text])
  } deriving (Generic, Show, Eq)

data MultiContext = MultiContext
  { fullKey :: !Text
  , contexts :: ![SingleContext]
  } deriving (Generic, Show, Eq)

contextSetName :: Text -> ContextI -> ContextI
contextSetName name (Single c) = Single $ setField @"name" (Just name) c
contextSetName _ c = c

contextSetAnonymous :: Bool -> ContextI -> ContextI
contextSetAnonymous anonymous (Single c) = Single $ setField @"anonymous" anonymous c
contextSetAnonymous _ c = c

contextSetAttribute :: Text -> Value -> ContextI -> ContextI
contextSetAttribute attribute value (Single c) =
  Single $ setField @"attributes" (Just attributes) c
  where attributes = case getField @"attributes" c of
                      Nothing -> singleton attribute value
                      Just a -> insertKey attribute value a
contextSetAttribute _ _ c = c

contextSetPrivateAttributes :: [Text] -> ContextI -> ContextI
contextSetPrivateAttributes privateAttributes (Single c) = Single $ setField @"privateAttributes" (Just privateAttributes) c
contextSetPrivateAttributes _ c = c

canonicalizeKey :: Text -> Text -> Text
canonicalizeKey key "user" = key
canonicalizeKey key kind = kind <> ":" <> (replace "%" "%25" key & replace ":" "%3A")

fullyQualifiedKey :: ContextI -> Maybe Text
fullyQualifiedKey (Single c) = Just $ getField @"fullKey" c
fullyQualifiedKey (Multi c) = Just $ getField @"fullKey" c
fullyQualifiedKey (InvalidContext _) = Nothing

contextIsValid :: ContextI -> Bool
contextIsValid (InvalidContext _) = False
contextIsValid _ = True

contextError :: ContextI -> Maybe Text
contextError (InvalidContext e) = Just e
contextError _ = Nothing

unwrapSingleContext :: ContextI -> Maybe SingleContext
unwrapSingleContext (Single c) = Just c
unwrapSingleContext _ = Nothing

contextKind :: SingleContext -> Text
contextKind c = getField @"kind" c

contextKey :: SingleContext -> Text
contextKey c = getField @"key" c
