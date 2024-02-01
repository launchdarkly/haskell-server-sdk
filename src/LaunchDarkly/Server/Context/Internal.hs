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
module LaunchDarkly.Server.Context.Internal
    ( Context (..)
    , SingleContext (..)
    , MultiContext (..)
    , makeContext
    , makeMultiContext
    , withName
    , withAnonymous
    , withAttribute
    , withPrivateAttributes
    , getKey
    , getKeys
    , getCanonicalKey
    , getKinds
    , redactContext
    )
where

import Data.Aeson (FromJSON, Result (Success), ToJSON, Value (..), fromJSON, parseJSON, toJSON, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser, prependFailure, typeMismatch)
import Data.Function ((&))
import Data.Generics.Product (getField, setField)
import qualified Data.HashSet as HS
import Data.List (sortBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text, intercalate, replace, unpack)
import GHC.Generics (Generic)
import qualified GHC.Exts as Exts (fromList)
import LaunchDarkly.AesonCompat (KeyMap, deleteKey, emptyObject, foldrWithKey, fromList, insertKey, keyMapUnion, lookupKey, mapValues, objectKeys, singleton, toList)
import LaunchDarkly.Server.Config (Config)
import LaunchDarkly.Server.Reference (Reference)
import qualified LaunchDarkly.Server.Reference as R

-- | data record for the Context type
data Context
    = Single SingleContext
    | Multi MultiContext
    | Invalid {error :: !Text}
    deriving (Generic, Show, Eq)

instance ToJSON Context where
    toJSON (Single c) = toJSON c
    toJSON (Multi c) = toJSON c
    toJSON (Invalid c) = toJSON c

instance FromJSON Context where
    parseJSON a@(Object o) =
        case lookupKey "kind" o of
            Nothing -> parseLegacyUser a
            Just (String "multi") -> parseMultiContext a
            Just _ -> parseSingleContext a
    parseJSON invalid = prependFailure "parsing Context failed, " (typeMismatch "Object" invalid)

data SingleContext = SingleContext
    { key :: !Text
    , fullKey :: !Text
    , kind :: !Text
    , name :: !(Maybe Text)
    , anonymous :: !Bool
    , attributes :: !(Maybe (KeyMap Value))
    , privateAttributes :: !(Maybe (Set Reference))
    }
    deriving (Generic, Show, Eq)

instance ToJSON SingleContext where
    toJSON = (toJsonObject True)

data MultiContext = MultiContext
    { fullKey :: !Text
    , contexts :: !(KeyMap SingleContext)
    }
    deriving (Generic, Show, Eq)

instance ToJSON MultiContext where
    toJSON (MultiContext {contexts}) =
        mapValues (\c -> toJsonObject False c) contexts
            & insertKey "kind" "multi"
            & Object

-- |
-- Create a single kind context from the provided hash.
--
-- The provided hash must match the format as outlined in the [SDK
-- documentation](https://docs.launchdarkly.com/sdk/features/user-config).
makeContext :: Text -> Text -> Context
makeContext "" _ = Invalid {error = "context key must not be empty"}
makeContext key kind = makeSingleContext key kind

-- This function is used internally to create a context with legacy key
-- validation rules; namely, a legacy context is allowed to have an empty key.
-- No other type of context is. Users of this SDK can only use the makeContext
-- to create a single-kind context, which includes the non-empty key
-- restriction.
makeSingleContext :: Text -> Text -> Context
makeSingleContext _ "" = Invalid {error = "context kind must not be empty"}
makeSingleContext _ "kind" = Invalid {error = "context kind cannot be 'kind'"}
makeSingleContext _ "multi" = Invalid {error = "context kind cannot be 'multi'"}
makeSingleContext key kind
    | (all (`elem` ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['.', '-', '_']) (unpack kind)) == False = Invalid {error = "context kind contains disallowed characters"}
    | otherwise =
        Single
            SingleContext
                { key = key
                , fullKey = canonicalizeKey key kind
                , kind = kind
                , name = Nothing
                , anonymous = False
                , attributes = Nothing
                , privateAttributes = Nothing
                }

-- |
-- Create a multi-kind context from the list of Contexts provided.
--
-- A multi-kind context is comprised of two or more single kind contexts. You
-- cannot include a multi-kind context instead another multi-kind context.
--
-- Additionally, the kind of each single-kind context must be unique. For
-- instance, you cannot create a multi-kind context that includes two user kind
-- contexts.
--
-- If you attempt to create a multi-kind context from one single-kind context,
-- this method will return the single-kind context instead of a new multi-kind
-- context wrapping that one single-kind.
makeMultiContext :: [Context] -> Context
makeMultiContext [] = Invalid {error = "multi-kind contexts require at least one single-kind context"}
makeMultiContext [c@(Single _)] = c
makeMultiContext contexts =
    let singleContexts = mapMaybe unwrapSingleContext contexts
        sorted = sortBy (\lhs rhs -> compare (kind lhs) (kind rhs)) singleContexts
        kinds = HS.fromList $ map kind singleContexts
     in case (length contexts, length singleContexts, length kinds) of
            (a, b, _) | a /= b -> Invalid {error = "multi-kind contexts can only contain single-kind contexts"}
            (a, _, c) | a /= c -> Invalid {error = "multi-kind contexts cannot contain two single-kind contexts with the same kind"}
            _ ->
                Multi
                    MultiContext
                        { fullKey = intercalate ":" $ map (\c -> canonicalizeKey (key c) (kind c)) sorted
                        , contexts = fromList $ map (\c -> ((kind c), c)) singleContexts
                        }

-- |
-- Sets the name attribute for a single-kind context.
--
-- Calling this method on an invalid or multi-kind context is a no-op.
withName :: Text -> Context -> Context
withName name (Single c) = Single $ setField @"name" (Just name) c
withName _ c = c

-- |
-- Sets the anonymous attribute for a single-kind context.
--
-- Calling this method on an invalid or multi-kind context is a no-op.
withAnonymous :: Bool -> Context -> Context
withAnonymous anonymous (Single c) = Single $ setField @"anonymous" anonymous c
withAnonymous _ c = c

-- |
-- Sets the value of any attribute for the context.
--
-- This includes only attributes that are addressable in evaluations -- not
-- metadata such as private attributes. For example, if the attribute name is
-- "privateAttributes", you will be setting an attribute with that name which
-- you can use in evaluations or to record data for your own purposes, but it
-- will be unrelated to 'withPrivateAttributes'.
--
-- If attribute name is "privateAttributeNames", it is ignored and no attribute
-- is set.
--
-- This method uses the Value type to represent a value of any JSON type: null,
-- boolean, number, string, array, or object. For all attribute names that do
-- not have special meaning to LaunchDarkly, you may use any of those types.
-- Values of different JSON types are always treated as different values: for
-- instance, null, false, and the empty string "" are not the same, and the
-- number 1 is not the same as the string "1".
--
-- The following attribute names have special restrictions on their value
-- types, and any value of an unsupported type will be ignored (leaving the
-- attribute unchanged):
--
-- - "name": Must be a string.
-- - "anonymous": Must be a boolean.
--
-- The attribute name "_meta" is not allowed, because it has special meaning in
-- the JSON schema for contexts; any attempt to set an attribute with this name
-- has no effect.
--
-- The attribute names "kind" and "key" are not allowed. They must be provided
-- during the initial context creation. See 'makeContext'.
--
-- Values that are JSON arrays or objects have special behavior when referenced
-- in flag/segment rules.
--
-- For attributes that aren't subject to the special restrictions mentioned
-- above, a value of Null is equivalent to removing any current non-default
-- value of the attribute. Null is not a valid attribute value in the
-- LaunchDarkly model; any expressions in feature flags that reference an
-- attribute with a null value will behave as if the attribute did not exist.
--
-- Calling this method on an invalid or multi-kind context is a no-op.
withAttribute :: Text -> Value -> Context -> Context
withAttribute "key" _ c = c
withAttribute "kind" _ c = c
withAttribute "name" (String value) c = withName value c
withAttribute "name" Null (Single c) = Single $ c {name = Nothing}
withAttribute "name" _ c = c
withAttribute "anonymous" (Bool value) c = withAnonymous value c
withAttribute "anonymous" _ c = c
withAttribute "_meta" _ c = c
withAttribute "privateAttributeNames" _ c = c
withAttribute _ Null c@(Single SingleContext {attributes = Nothing}) = c
withAttribute attr value (Single c@(SingleContext {attributes = Nothing})) =
    Single $ c {attributes = Just $ singleton attr value}
withAttribute attr Null (Single c@(SingleContext {attributes = Just attrs})) =
    Single $ c {attributes = Just $ deleteKey attr attrs}
withAttribute attr value (Single c@(SingleContext {attributes = Just attrs})) =
    Single $ c {attributes = Just $ insertKey attr value attrs}
withAttribute _ _ c = c

-- |
-- Sets the private attributes for a single-kind context.
--
-- Calling this method on an invalid or multi-kind context is a no-op.
withPrivateAttributes :: Set Reference -> Context -> Context
withPrivateAttributes attrs (Single c)
    | S.null attrs = Single $ c {privateAttributes = Nothing}
    | otherwise = Single $ c {privateAttributes = Just attrs}
withPrivateAttributes _ c = c

-- Given a key and kind, generate a canonical key.
--
-- In a multi-kind context, each individual context should theoretically
-- contain the same key. To address this restriction, we generate a canonical
-- key that includes the context's kind. However, if the kind is "user", we
-- omit the kind inclusion to maintain backwards compatibility.
canonicalizeKey :: Text -> Text -> Text
canonicalizeKey key "user" = key
canonicalizeKey key kind = kind <> ":" <> (replace "%" "%25" key & replace ":" "%3A")

unwrapSingleContext :: Context -> Maybe SingleContext
unwrapSingleContext (Single c) = Just c
unwrapSingleContext _ = Nothing

-- Internally used convenience function to retrieve a context's key.
--
-- This method is functionally equivalent to @fromMaybe "" $ getValue "key"@,
-- it's just nicer to use.
getKey :: Context -> Text
getKey (Single c) = key c
getKey _ = ""

-- Internally used convenience function for retrieving all context keys,
-- indexed by their kind.
--
-- A single kind context will return a single element map containing its kind
-- and key. Multi-kind contexts will return a map of kind / key pairs for each
-- of its sub-contexts. An invalid context will return the empty map.
getKeys :: Context -> KeyMap Text
getKeys (Single c) = singleton (kind c) (key c)
getKeys (Multi (MultiContext {contexts})) = mapValues key contexts
getKeys _ = emptyObject

-- Internally used convenience function to retrieve a context's fully qualified
-- key.
getCanonicalKey :: Context -> Text
getCanonicalKey (Single c) = getField @"fullKey" c
getCanonicalKey (Multi c) = getField @"fullKey" c
getCanonicalKey _ = ""

-- Internally used convenience function for retrieving a list of context kinds
-- in the provided context.
--
-- A single kind context will return a single element list containing only that
-- one kind. Multi-kind contexts will return a list of kinds for each of its
-- sub-contexts. An invalid context will return the empty list.
getKinds :: Context -> [Text]
getKinds (Single c) = [kind c]
getKinds (Multi (MultiContext {contexts})) = objectKeys contexts
getKinds _ = []

-- Internally used function for encoding a SingleContext into a JSON object.
--
-- This functionality has been extracted into this separate function because we
-- need to control whether or not the kind property will be included in the
-- final output. If we didn't have this restriction, we could simply inline
-- this function on the SingleContext.
toJsonObject :: Bool -> SingleContext -> Value
toJsonObject includeKind context =
    Object $ fromList $ (getMapOfRedactableProperties context ++ getMapOfRequiredProperties includeKind context)

-- Contexts can be broken into two different types of attributes -- those which
-- can be redacted, and those which can't.
--
-- This method will return a list of name / value pairs which represent the
-- attributes which are eligible for redaction. The other half of the context
-- can be retrieved through the getMapOfRequiredProperties function.
getMapOfRedactableProperties :: SingleContext -> [(Text, Value)]
getMapOfRedactableProperties (SingleContext {name = Nothing, attributes = Nothing}) = []
getMapOfRedactableProperties (SingleContext {name = Nothing, attributes = Just attrs}) = toList attrs
getMapOfRedactableProperties (SingleContext {name = Just n, attributes = Just attrs}) = ("name", String n) : (toList attrs)
getMapOfRedactableProperties (SingleContext {name = Just n, attributes = Nothing}) = [("name", String n)]

-- Contexts can be broken into two different types of attributes -- those which
-- can be redacted, and those which can't.
--
-- This method will return a list of name / value pairs which represent the
-- attributes which cannot be redacted. The other half of the context can be
-- retrieved through the getMapOfRedactableProperties function.
getMapOfRequiredProperties :: Bool -> SingleContext -> [(Text, Value)]
getMapOfRequiredProperties includeKind SingleContext {key, kind, anonymous, privateAttributes} =
    filter
        ((/=) Null . snd)
        [ ("key", toJSON $ key)
        , ("kind", toJSON $ if includeKind then String kind else Null)
        , ("anonymous", toJSON $ if anonymous then Bool True else Null)
        , ("_meta", maybe Null toJSON privateAttributes)
        ,
            ( "_meta"
            , case privateAttributes of
                Nothing -> Null
                Just attrs -> toJSON $ singleton "privateAttributes" (Array $ Exts.fromList $ map toJSON $ S.elems attrs)
            )
        ]

-- Internally used function to decode a JSON object using the legacy user
-- scheme into a modern single-kind "user" context.
parseLegacyUser :: Value -> Parser Context
parseLegacyUser = withObject "LegacyUser" $ \o -> do
    (key :: Text) <- o .: "key"
    (secondary :: Maybe Text) <- o .:? "secondary"
    (ip :: Maybe Text) <- o .:? "ip"
    (country :: Maybe Text) <- o .:? "country"
    (email :: Maybe Text) <- o .:? "email"
    (firstName :: Maybe Text) <- o .:? "firstName"
    (lastName :: Maybe Text) <- o .:? "lastName"
    (avatar :: Maybe Text) <- o .:? "avatar"
    (name :: Maybe Text) <- o .:? "name"
    (anonymous :: Maybe Bool) <- o .:? "anonymous"
    (custom :: Maybe (KeyMap Value)) <- o .:? "custom"
    (privateAttributeNames :: Maybe [Text]) <- o .:? "privateAttributeNames"
    let context =
            makeSingleContext key "user"
                & withAttribute "secondary" (fromMaybe Null (String <$> secondary))
                & withAttribute "ip" (fromMaybe Null (String <$> ip))
                & withAttribute "country" (fromMaybe Null (String <$> country))
                & withAttribute "email" (fromMaybe Null (String <$> email))
                & withAttribute "firstName" (fromMaybe Null (String <$> firstName))
                & withAttribute "lastName" (fromMaybe Null (String <$> lastName))
                & withAttribute "avatar" (fromMaybe Null (String <$> avatar))
                & withAttribute "name" (fromMaybe Null (String <$> name))
                & withAttribute "anonymous" (fromMaybe Null (Bool <$> anonymous))
                & withPrivateAttributes (S.fromList $ map R.makeLiteral $ fromMaybe [] privateAttributeNames)
     in return $ foldrWithKey (\k v c -> withAttribute k v c) context (fromMaybe emptyObject custom)

-- Internally used function to decode a JSON object using the new context
-- scheme into a modern single-kind context.
parseSingleContext :: Value -> Parser Context
parseSingleContext = withObject "SingleContext" $ \o -> do
    (key :: Text) <- o .: "key"
    (kind :: Text) <- o .: "kind"
    (meta :: Maybe (KeyMap Value)) <- o .:? "_meta"
    (privateAttributes :: Maybe [Text]) <- (fromMaybe emptyObject meta) .:? "privateAttributes"
    let context =
            makeContext key kind
                & withPrivateAttributes (S.fromList $ map R.makeReference $ fromMaybe [] privateAttributes)
     in return $ foldrWithKey (\k v c -> withAttribute k v c) context o

-- Internally used function to decode a JSON object using the new context
-- scheme into a modern multi-kind context.
parseMultiContext :: Value -> Parser Context
parseMultiContext = withObject "MultiContext" $ \o -> do
    let contextLists = toList $ deleteKey "kind" o
        contextObjectLists = mapMaybe (\(k, v) -> case (k, v) of (_, Object obj) -> Just (k, obj); _ -> Nothing) contextLists
        results = map (\(kind, obj) -> fromJSON $ Object $ insertKey "kind" (String kind) obj) contextObjectLists
        single = mapMaybe (\result -> case result of Success r -> Just r; _ -> Nothing) results
     in case (length contextLists, length single) of
            (a, b) | a /= b -> return $ Invalid {error = "multi-kind context JSON contains non-single-kind contexts"}
            (_, _) -> return $ makeMultiContext single

-- Internally used function which performs context attribute redaction.
redactContext :: Config -> Context -> Value
redactContext _ (Invalid _) = Null
redactContext config (Multi MultiContext {contexts}) =
    mapValues (\context -> redactSingleContext False context (getAllPrivateAttributes config context)) contexts
        & insertKey "kind" "multi"
        & Object
        & toJSON
redactContext config (Single context) =
    toJSON $ redactSingleContext True context (getAllPrivateAttributes config context)

-- Apply redaction requirements to a SingleContext type.
redactSingleContext :: Bool -> SingleContext -> Set Reference -> Value
redactSingleContext includeKind context privateAttributes =
    let State {context = redactedContext, redacted} = foldr applyRedaction State {context = fromList $ getMapOfRedactableProperties context, redacted = []} privateAttributes
        redactedValues = Array $ Exts.fromList $ map String redacted
        required = fromList $ getMapOfRequiredProperties includeKind context
     in case redacted of
            [] -> Object $ keyMapUnion redactedContext required
            _ -> Object $ keyMapUnion redactedContext (insertKey "_meta" (Object $ singleton "redactedAttributes" redactedValues) required)

-- Internally used convenience function for creating a Set of References which
-- can redact all top level values in a provided context.
--
-- Given the context:
-- {
--      "kind": "user",
--      "key": "user-key",
--      "name": "Sandy",
--      "address": {
--          "city": "Chicago"
--      }
-- }
--
-- getAllTopLevelRedactableNames context would yield the set ["name",
-- "address"].
getAllTopLevelRedactableNames :: SingleContext -> Set Reference
getAllTopLevelRedactableNames SingleContext {name = Nothing, attributes = Nothing} = S.empty
getAllTopLevelRedactableNames SingleContext {name = Just _, attributes = Nothing} = S.singleton $ R.makeLiteral "name"
getAllTopLevelRedactableNames SingleContext {name = Nothing, attributes = Just attrs} = S.fromList $ map R.makeLiteral $ objectKeys attrs
getAllTopLevelRedactableNames SingleContext {name = Just _, attributes = Just attrs} = S.fromList $ (R.makeLiteral "name") : (map R.makeLiteral $ objectKeys attrs)

-- Internally used convenience function to return a set of references which
-- would apply all redaction rules.
--
-- If allAttributesPrivate is True in the config, this will return a set which
-- covers the entire context.
getAllPrivateAttributes :: Config -> SingleContext -> Set Reference
getAllPrivateAttributes (getField @"allAttributesPrivate" -> True) context = getAllTopLevelRedactableNames context
getAllPrivateAttributes config SingleContext {privateAttributes = Nothing} = getField @"privateAttributeNames" config
getAllPrivateAttributes config SingleContext {privateAttributes = Just attrs} = S.union (getField @"privateAttributeNames" config) attrs

-- Internally used storage type for returning both the resulting redacted
-- context and the list of any attributes which were redacted.
data State = State
    { context :: KeyMap Value
    , redacted :: ![Text]
    }

-- Internally used store type for managing some state while the redaction
-- process is recursing.
data RedactState = RedactState
    { context :: KeyMap Value
    , reference :: Reference
    , redacted :: ![Text]
    }

-- Kick off the redaction process by priming the recursive redaction state.
applyRedaction :: Reference -> State -> State
applyRedaction reference State {context, redacted} =
    let (RedactState {context = c, redacted = r}) = redactComponents (R.getComponents reference) 0 RedactState {context, redacted, reference}
     in State {context = c, redacted = r}

-- Recursively apply redaction rules
redactComponents :: [Text] -> Int -> RedactState -> RedactState
-- If there are no components left to explore, then we can just return the
-- current state of things. This branch should never actually execute.
-- References aren't valid if there isn't at least one component, and we don't
-- recurse in the single component case. We just include it here for
-- completeness.
redactComponents [] _ state = state
-- kind, key, and anonymous are top level attributes that cannot be redacted.
redactComponents ["kind"] 0 state = state
redactComponents ["key"] 0 state = state
redactComponents ["anonymous"] 0 state = state
-- If we have a single component, then we are either trying to redact a simple
-- top level item, or we have recursed through all reference component parts
-- until the last one. We determine which of those situations we are in through
-- use of the 'level' parameter. 'level' = 0 means we are at the top level of
-- the call stack.
--
-- If we have a single component and we have found it in the current context
-- map, then we know we can redact it.
--
-- If we do not find it in the context, but we are at the top level (and thus
-- making a simple redaction), we consider that a successful redaction.
--
-- Otherwise, if there is no match and we aren't at the top level, the
-- redaction has failed and so we can just return the current state unmodified.
redactComponents [x] level state@(RedactState {context, reference, redacted}) = case (level, lookupKey x context) of
    (_, Just _) -> state {context = deleteKey x context, redacted = (R.getRawPath reference) : redacted}
    (0, _) -> state {redacted = (R.getRawPath reference) : redacted}
    _ -> state
redactComponents (x : xs) level state@(RedactState {context}) = case lookupKey x context of
    Just (Object o) ->
        let substate@(RedactState {context = subcontext}) = redactComponents xs (level + 1) (state {context = o})
         in substate {context = insertKey x (Object $ subcontext) context}
    _ -> state
