{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Context is a collection of attributes that can be referenced in flag
-- evaluations and analytics events.
--
-- To create a Context of a single kind, such as a user, you may use
-- 'makeContext'.
--
-- To create an LDContext with multiple kinds, use 'makeMultiContext'.
--
-- Additional properties can be set on a single-kind context using the set
-- methods found in this module.
--
-- Each method will always return a Context. However, that Context may be
-- invalid. You can check the validity of the resulting context, and the
-- associated errors by calling 'isValid' and 'getError'.
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
    , getValueForReference
    , getValue
    )
where

import Data.Aeson (Value (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import LaunchDarkly.AesonCompat (lookupKey)
import LaunchDarkly.Server.Context.Internal (Context (..), MultiContext (..), SingleContext (..), makeContext, makeMultiContext, withAnonymous, withAttribute, withName, withPrivateAttributes)
import LaunchDarkly.Server.Reference (Reference)
import qualified LaunchDarkly.Server.Reference as R

-- | Determines if the provided context is valid.
isValid :: Context -> Bool
isValid (Invalid _) = False
isValid _ = True

-- | Returns the error associated with the context if it is invalid.
getError :: Context -> Text
getError (Invalid e) = e
getError _ = ""

-- |
-- Returns the single-kind Context corresponding to one of the kinds in this
-- context.
--
-- If this method is called on a single-kind Context and the requested kind
-- matches the context's kind, then that context is returned.
--
-- If the method is called on a multi-context, the provided kind must match the
-- context kind of one of the individual contexts.
--
-- If there is no context corresponding to `kind`, the method returns Nothing.
getIndividualContext :: Text -> Context -> Maybe Context
getIndividualContext kind (Multi (MultiContext {contexts})) = Single <$> lookupKey kind contexts
getIndividualContext kind c@(Single (SingleContext {kind = k}))
    | kind == k = Just c
    | otherwise = Nothing
getIndividualContext _ _ = Nothing

-- |
-- Looks up the value of any attribute of the Context by name. This includes
-- only attributes that are addressable in evaluations-- not metadata such as
-- private attributes.
--
-- For a single-kind context, the attribute name can be any custom attribute.
-- It can also be one of the built-in ones like "kind", "key", or "name".
--
-- For a multi-kind context, the only supported attribute name is "kind". Use
-- 'getIndividualContext' to inspect a Context for a particular kind and then
-- get its attributes.
--
-- This method does not support complex expressions for getting individual
-- values out of JSON objects or arrays, such as "/address/street". Use
-- 'getValueForReference' for that purpose.
--
-- If the value is found, the return value is the attribute value; otherwise,
-- it is Null.
getValue :: Text -> Context -> Value
getValue ref = getValueForReference (R.makeLiteral ref)

-- |
-- Looks up the value of any attribute of the Context, or a value contained
-- within an attribute, based on a 'Reference' instance. This includes only
-- attributes that are addressable in evaluations-- not metadata such as
-- private attributes.
--
-- This implements the same behavior that the SDK uses to resolve attribute
-- references during a flag evaluation. In a single-kind context, the
-- 'Reference' can represent a simple attribute name-- either a built-in one
-- like "name" or "key", or a custom attribute -- or, it can be a
-- slash-delimited path using a JSON-Pointer-like syntax. See 'Reference' for
-- more details.
--
-- For a multi-kind context, the only supported attribute name is "kind". Use
-- 'getIndividualContext' to inspect a Context for a particular kind and then
-- get its attributes.
--
-- If the value is found, the return value is the attribute value; otherwise,
-- it is Null.
getValueForReference :: Reference -> Context -> Value
getValueForReference (R.isValid -> False) _ = Null
getValueForReference reference context = case R.getComponents reference of
    [] -> Null
    (component : components) ->
        let value = getTopLevelValue component context
         in foldl getValueFromJsonObject value components

-- This helper method retrieves a Value from a JSON object type.
--
-- If the key does not exist, or the type isn't an object, this method will
-- return Null.
getValueFromJsonObject :: Value -> Text -> Value
getValueFromJsonObject (Object nm) component = fromMaybe Null (lookupKey component nm)
getValueFromJsonObject _ _ = Null

-- Attribute retrieval can mostly be defined recursively. However, this isn't
-- true for the top level attribute since the entire context isn't stored in a
-- single object property.
--
-- To prime the recursion, we define this simple helper function to retrieve
-- attributes addressable at the top level.
getTopLevelValue :: Text -> Context -> Value
getTopLevelValue _ (Invalid _) = Null
getTopLevelValue "kind" (Multi _) = "multi"
getTopLevelValue _ (Multi _) = Null
getTopLevelValue "key" (Single SingleContext {key}) = String key
getTopLevelValue "kind" (Single SingleContext {kind}) = String kind
getTopLevelValue "name" (Single SingleContext {name = Nothing}) = Null
getTopLevelValue "name" (Single SingleContext {name = Just n}) = String n
getTopLevelValue "anonymous" (Single SingleContext {anonymous}) = Bool anonymous
getTopLevelValue _ (Single SingleContext {attributes = Nothing}) = Null
getTopLevelValue key (Single SingleContext {attributes = Just attrs}) = fromMaybe Null $ lookupKey key attrs
