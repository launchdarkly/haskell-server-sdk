-- | Context is a collection of attributes that can be referenced in flag evaluations and analytics events.
--
-- To create a Context of a single kind, such as a user, you may use 'makeContext'.
--
-- To create an LDContext with multiple kinds, use 'makeMultiContext'.
--
-- Additional properties can be set on a single-kind context using the set methods found in this module.
--
-- Each method will always return a Context. However, that Context may be invalid. You can check the validity of the
-- resulting context, and the associated errors by calling 'isValid' and 'getError'.

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module LaunchDarkly.Server.Context
  ( Context
  , makeContext
  , makeMultiContext
  , withName
  , withAnonymous
  , withAttribute
  , withPrivateAttributes
  , isValid
  , getError
  , getIndividualContext
  , getValue
  , getValueForReference
  )

where

import Data.Aeson (Value(..))
import Data.Text (Text, unpack, replace)
import Data.Maybe (mapMaybe, fromMaybe)

import LaunchDarkly.Server.Reference (Reference)
import qualified LaunchDarkly.Server.Reference as R
import qualified Data.HashSet as HS
import Data.List (sortBy)
import Data.Text (intercalate)
import LaunchDarkly.AesonCompat (KeyMap, singleton, insertKey, lookupKey)
import Data.Generics.Product (setField)
import GHC.Generics (Generic)
import Data.Function ((&))

-- | data record for the Context type
data Context =
  Single SingleContext
  | Multi MultiContext
  | Invalid { error :: !Text } deriving (Show, Eq)

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

-- | Create a single kind context from the provided hash.
--
-- The provided hash must match the format as outlined in the
-- [SDK documentation](https://docs.launchdarkly.com/sdk/features/user-config).
makeContext :: Text -> Text -> Context
makeContext "" _ = Invalid { error = "context key must not be empty" }
makeContext _ "" = Invalid { error = "context kind must not be empty" }
makeContext _ "kind" = Invalid { error = "context kind cannot be 'kind'" }
makeContext _ "multi" = Invalid { error = "context kind cannot be 'multi'" }
makeContext key kind
  | (all (`elem` ['a'..'z'] ++ ['.', '-']) (unpack kind)) == False = Invalid { error = "context kind contains disallowed characters" }
  | otherwise = Single SingleContext
    { key = key
    , fullKey = canonicalizeKey key kind
    , kind = kind
    , name = Nothing
    , anonymous = False
    , attributes = Nothing
    , privateAttributes = Nothing
    }

-- | Create a multi-kind context from the list of Contexts provided.
--
-- A multi-kind context is comprised of two or more single kind contexts. You cannot include a multi-kind context
-- instead another multi-kind context.
--
-- Additionally, the kind of each single-kind context must be unique. For instance, you cannot create a multi-kind
-- context that includes two user kind contexts.
--
-- If you attempt to create a multi-kind context from one single-kind context, this method will return the single-kind
-- context instead of a new multi-kind context wrapping that one single-kind.
makeMultiContext :: [Context] -> Context
makeMultiContext [] = Invalid { error = "multi-kind contexts require at least one single-kind context" }
makeMultiContext [c@(Single _)] = c
makeMultiContext contexts =
  let singleContexts = mapMaybe unwrapSingleContext contexts
      sorted = sortBy (\lhs rhs -> compare (kind lhs) (kind rhs)) singleContexts
      kinds = HS.fromList $ map kind singleContexts
  in case (length contexts, length singleContexts, length kinds) of
    (a, b, _) | a /= b -> Invalid { error = "multi-kind contexts can only contain single-kind contexts" }
    (a, _, c) | a /= c -> Invalid { error = "multi-kind contexts cannot contain two single-kind contexts with the same kind" }
    _ -> Multi MultiContext
      { fullKey =  intercalate ":" $ map (\c -> canonicalizeKey (key c) (kind c) ) sorted
      , contexts = singleContexts
      }

-- | Sets the name attribute for a a single-kind context.
--
-- Calling this method on an invalid or multi-kind context is a no-op.
withName :: Text -> Context -> Context
withName name (Single c) = Single $ setField @"name" (Just name) c
withName _ c = c

-- | Sets the anonymous attribute for a a single-kind context.
--
-- Calling this method on an invalid or multi-kind context is a no-op.
withAnonymous :: Bool -> Context -> Context
withAnonymous anonymous (Single c) = Single $ setField @"anonymous" anonymous c
withAnonymous _ c = c

-- | Sets the value of any attribute for the context.
--
-- This includes only attributes that are addressable in evaluations -- not metadata such as private attributes. For
-- example, if the attribute name is "privateAttributes", you will be setting an attribute with that name which you can
-- use in evaluations or to record data for your own purposes, but it will be unrelated to
-- 'withPrivateAttributes'.
--
-- If attribute name is "privateAttributeNames", it is ignored and no attribute is set.
--
-- This method uses the Value type to represent a value of any JSON type: null, boolean, number, string, array, or
-- object. For all attribute names that do not have special meaning to LaunchDarkly, you may use any of those types.
-- Values of different JSON types are always treated as different values: for instance, null, false, and the empty
-- string "" are not the same, and the number 1 is not the same as the string "1".
--
-- The following attribute names have special restrictions on their value types, and any value of an unsupported type
-- will be ignored (leaving the attribute unchanged):
--
-- - "name": Must be a string.
-- - "anonymous": Must be a boolean.
--
-- The attribute name "_meta" is not allowed, because it has special meaning in the JSON schema for contexts; any
-- attempt to set an attribute with this name has no effect.
--
-- The attribute names "kind" and "key" are not allowed. They must be provided during the initial context creation. See
-- 'makeContext'.
--
-- Values that are JSON arrays or objects have special behavior when referenced in flag/segment rules.
--
-- For attributes that aren't subject to the special restrictions mentioned above, a value of Null is equivalent to
-- removing any current non-default value of the attribute. Null is not a valid attribute value in the LaunchDarkly
-- model; any expressions in feature flags that reference an attribute with a null value will behave as if the attribute
-- did not exist.
--
-- Calling this method on an invalid or multi-kind context is a no-op.
withAttribute :: Text -> Value -> Context -> Context
withAttribute "key" _ c = c
withAttribute "kind" _ c = c
withAttribute "name" (String value) c = withName value c
withAttribute "name" _ c = c
withAttribute "anonymous" (Bool value) c = withAnonymous value c
withAttribute "anonymous" _ c = c
withAttribute "_meta" _ c = c
withAttribute "privateAttributeNames" _ c = c
withAttribute attr value (Single c@(SingleContext { attributes = Nothing })) =
  Single $ c { attributes = (Just $ singleton attr value) }
withAttribute attr value (Single c@(SingleContext { attributes = Just attrs })) =
  Single $ c { attributes = (Just $ insertKey attr value attrs) }
withAttribute _ _ c = c

-- | Sets the private attributes for a a single-kind context.
--
-- Calling this method on an invalid or multi-kind context is a no-op.
withPrivateAttributes :: [Text] -> Context -> Context
withPrivateAttributes attrs (Single c) = Single $ c { privateAttributes = Just attrs }
withPrivateAttributes _ c = c

-- | Determines if the provided context is valid.
isValid :: Context -> Bool
isValid (Invalid _) = False
isValid _ = True

-- | Returns the error associated with the context if it is invalid.
getError :: Context -> Maybe Text
getError (Invalid e) = Just e
getError _ = Nothing

-- TODO: Come back and implement this when needed.
getIndividualContext :: Text -> Maybe Context
getIndividualContext _ = undefined

-- | Looks up the value of any attribute of the Context by name. This includes only attributes that are addressable in
-- evaluations-- not metadata such as private attributes.
--
-- For a single-kind context, the attribute name can be any custom attribute. It can also be one of the built-in ones
-- like "kind", "key", or "name".
--
-- For a multi-kind context, the only supported attribute name is "kind". Use 'getIndividualContext' to inspect a
-- Context for a particular kind and then get its attributes.
--
-- This method does not support complex expressions for getting individual values out of JSON objects or arrays, such as
-- "/address/street". Use 'getValueForReference' for that purpose.
--
-- If the value is found, the return value is the attribute value; otherwise, it is Null.
getValue :: Text -> Context -> Value
getValue ref = getValueForReference (R.makeLiteral ref)

-- | Looks up the value of any attribute of the Context, or a value contained within an attribute, based on a
-- 'Reference' instance. This includes only attributes that are addressable in evaluations-- not metadata such as
-- private attributes.
--
-- This implements the same behavior that the SDK uses to resolve attribute references during a flag evaluation. In a
-- single-kind context, the 'Reference' can represent a simple attribute name-- either a built-in one like "name" or
-- "key", or a custom attribute -- or, it can be a slash-delimited path using a JSON-Pointer-like syntax. See
-- 'Reference' for more details.
--
-- For a multi-kind context, the only supported attribute name is "kind". Use 'getIndividualContext' to inspect a
-- Context for a particular kind and then get its attributes.
--
-- If the value is found, the return value is the attribute value; otherwise, it is Null.
getValueForReference :: Reference -> Context -> Value
getValueForReference (R.isValid -> False) _ = Null
getValueForReference reference context = case R.getComponents reference of
    [] -> Null
    (component:components) ->
      let value = getTopLevelValue component context
      in foldl getValueFromJsonObject value components

-- This helper method retrieves a Value from a JSON object type.
--
-- If the key does not exist, or the type isn't an object, this method will return Null.
getValueFromJsonObject :: Value -> Text -> Value
getValueFromJsonObject (Object nm) component = fromMaybe Null (lookupKey component nm)
getValueFromJsonObject _ _ = Null

-- Attribute retrieval can mostly be defined recursively. However, this isn't true for the top level attribute since
-- the entire context isn't stored in a single object property.
--
-- To prime the recursion, we define this simple helper function to retrieve attributes addressable at the top level.
getTopLevelValue :: Text -> Context -> Value
getTopLevelValue _ (Invalid _) = Null
getTopLevelValue "kind" (Multi _) = "multi"
getTopLevelValue _ (Multi _) = Null
getTopLevelValue "key" (Single SingleContext { key }) = String key
getTopLevelValue "kind" (Single SingleContext { kind }) = String kind
getTopLevelValue "name" (Single SingleContext { name = Nothing}) = Null
getTopLevelValue "name" (Single SingleContext { name = Just n}) = String n
getTopLevelValue "anonymous" (Single SingleContext { anonymous }) = Bool anonymous
getTopLevelValue _ (Single SingleContext { attributes = Nothing }) = Null
getTopLevelValue key (Single SingleContext { attributes = Just attrs }) = fromMaybe Null $ lookupKey key attrs

-- Given a key and kind, generate a canonical key.
--
-- In a multi-kind context, each individual context should theoretically contain the same key. To address this
-- restriction, we generate a canonical key that includes the context's kind. However, if the kind is "user", we omit
-- the kind inclusion to maintain backwards compatibility.
canonicalizeKey :: Text -> Text -> Text
canonicalizeKey key "user" = key
canonicalizeKey key kind = kind <> ":" <> (replace "%" "%25" key & replace ":" "%3A")

unwrapSingleContext :: Context -> Maybe SingleContext
unwrapSingleContext (Single c) = Just c
unwrapSingleContext _ = Nothing