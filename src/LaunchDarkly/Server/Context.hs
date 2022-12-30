-- | This module is for configuration of the context object.

module LaunchDarkly.Server.Context
  ( Context
  , makeContext
  , makeMultiContext
  , contextSetName
  , contextSetAnonymous
  , contextSetAttribute
  , contextSetPrivateAttributes
  , contextIsValid
  , contextError
  )

where

import Data.Aeson (Value(..))
import Data.Text (Text, unpack)
import Data.Maybe (mapMaybe)

import qualified LaunchDarkly.Server.Context.Internal as CTX
import LaunchDarkly.Server.Context.Internal (Context(..), ContextI(..), SingleContext(..), MultiContext(..))
import qualified Data.HashSet as HS
import Data.List (sortBy)
import Data.Text (intercalate)

-- | Create a single kind context from the provided hash.
--
-- The provided hash must match the format as outlined in the
-- [SDK documentation](https://docs.launchdarkly.com/sdk/features/user-config).
makeContext :: Text -> Text -> Context
makeContext "" _ = Context $ InvalidContext { error = "context key must not be empty" }
makeContext key kind
  | kind == "" = Context $ InvalidContext { error = "context kind must not be empty" }
  | kind == "kind" = Context $ InvalidContext { error = "context kind cannot be 'kind'" }
  | kind == "multi" = Context $ InvalidContext { error = "context kind cannot be 'multi'" }
  | (all (`elem` ['a'..'z'] ++ ['.', '-']) (unpack kind)) == False = Context $ InvalidContext { error = "context kind contains disallowed characters" }
  | otherwise = Context $ Single SingleContext
    { key = key
    , fullKey = CTX.canonicalizeKey key kind
    , kind = kind
    , name = Nothing
    , anonymous = False
    , attributes = Nothing
    , privateAttributes = Nothing
    }

-- | Create a multi-kind context from the list of Contexts provided.
--
-- A multi-kind context is comprised of two or more single kind contexts.
-- You cannot include a multi-kind context instead another multi-kind
-- context.
--
-- Additionally, the kind of each single-kind context must be unique. For
-- instance, you cannot create a multi-kind context that includes two user
-- kind contexts.
--
-- If you attempt to create a multi-kind context from one single-kind
-- context, this method will return the single-kind context instead of a new
-- multi-kind context wrapping that one single-kind.
makeMultiContext :: [Context] -> Context
makeMultiContext contexts =
  let singleContexts = mapMaybe (CTX.mapValue CTX.unwrapSingleContext) contexts
      sorted = sortBy (\lhs rhs -> compare (CTX.contextKind lhs) (CTX.contextKind rhs) ) singleContexts
      kinds = HS.fromList $ map CTX.contextKind singleContexts
  in case (length contexts, length singleContexts, length kinds) of
    (0, _, _) -> Context $ InvalidContext { error = "multi-kind contexts require at least one single-kind context" }
    (1, _, _) -> Context $ Single (head singleContexts)
    (a, b, _) | a /= b -> Context $ InvalidContext { error = "multi-kind contexts can only contain single-kind contexts" }
    (a, _, c) | a /= c -> Context $ InvalidContext { error = "multi-kind contexts cannot contain two single-kind contexts with the same kind" }
    _ -> Context $ Multi MultiContext
      { fullKey =  intercalate ":" $ map (\c -> CTX.canonicalizeKey (CTX.contextKey c) (CTX.contextKind c) ) sorted
      , contexts = singleContexts
      }

-- | Sets the name attribute for a a single-kind context.
--
-- Calling this method on an invalid or multi-kind context is a no-op.
contextSetName :: Text -> Context -> Context
contextSetName = CTX.mapContext . CTX.contextSetName

-- | Sets the anonymous attribute for a a single-kind context.
--
-- Calling this method on an invalid or multi-kind context is a no-op.
contextSetAnonymous :: Bool -> Context -> Context
contextSetAnonymous = CTX.mapContext . CTX.contextSetAnonymous

-- | Sets the value of any attribute for the context.
--
-- This includes only attributes that are addressable in evaluations -- not metadata such as
-- private attributes. For example, if `attribute_name` is "privateAttributes", you will be
-- setting an attribute with that name which you can use in evaluations or to record data for
-- your own purposes, but it will be unrelated to 'contextSetPrivateAttributes'.
--
-- If `attribute_name` is "privateAttributeNames", it is ignored and no
-- attribute is set.
--
-- This method uses the Value type to represent a value of any JSON type: null,
-- boolean, number, string, array, or object. For all attribute names that do not have special
-- meaning to LaunchDarkly, you may use any of those types. Values of different JSON types are
-- always treated as different values: for instance, null, false, and the empty string "" are
-- not the same, and the number 1 is not the same as the string "1".
--
-- The following attribute names have special restrictions on their value types, and any value
-- of an unsupported type will be ignored (leaving the attribute unchanged):
--
-- - "name": Must be a string.
-- - "anonymous": Must be a boolean.
--
-- The attribute name "_meta" is not allowed, because it has special meaning in the JSON
-- schema for contexts; any attempt to set an attribute with this name has no effect.
--
-- The attribute names "kind" and "key" are not allowed. They must be provided during the initial context creation. See
-- 'makeContext'.
--
-- Values that are JSON arrays or objects have special behavior when referenced in
-- flag/segment rules.
--
-- For attributes that aren't subject to the special restrictions mentioned above,
-- a value of Null is equivalent to removing any current non-default value
-- of the attribute. Null is not a valid attribute value in the LaunchDarkly model; any
-- expressions in feature flags that reference an attribute with a null value will behave as
-- if the attribute did not exist.
--
-- Calling this method on an invalid or multi-kind context is a no-op.
contextSetAttribute :: Text -> Value -> Context -> Context
-- TODO: We need to add some unit tests for this method once we have a way of retrieving values
contextSetAttribute "key" _ c = c
contextSetAttribute "kind" _ c = c
contextSetAttribute "name" (String value) c = contextSetName value c
contextSetAttribute "name" _ c = c
contextSetAttribute "anonymous" (Bool value) c = contextSetAnonymous value c
contextSetAttribute "anonymous" _ c = c
contextSetAttribute "_meta" _ c = c
contextSetAttribute "privateAttributeNames" _ c = c
contextSetAttribute attribute value c = CTX.mapContext (\ci -> CTX.contextSetAttribute attribute value ci) c

-- | Sets the name attribute for a a single-kind context.
--
-- Calling this method on an invalid or multi-kind context is a no-op.
contextSetPrivateAttributes :: [Text] -> Context -> Context
contextSetPrivateAttributes = CTX.mapContext . CTX.contextSetPrivateAttributes

-- | Determines if the provided context is valid.
contextIsValid :: Context -> Bool
contextIsValid = CTX.mapValue CTX.contextIsValid

-- | Returns the error associated with the context if it is invalid.
contextError :: Context -> Maybe Text
contextError = CTX.mapValue CTX.contextError