-- | Reference is an attribute name or path expression identifying a value within a Context.
--
-- This type is mainly intended to be used internally by LaunchDarkly SDK and service code, where efficiency is a major
-- concern so it's desirable to do any parsing or preprocessing just once. Applications are unlikely to need to use the
-- Reference type directly.
--
-- It can be used to retrieve a value with 'LaunchDarkly.Server.Context.getValueForReference' or to identify an
-- attribute or nested value that should be considered private.
--
-- Parsing and validation are done at the time that the Reference is constructed. If a Reference instance was created
-- from an invalid string, it is considered invalid. The error can be inspected with 'getError'.
--
-- == Syntax
--
-- The string representation of an attribute reference in LaunchDarkly JSON data uses the following syntax:
--
-- If the first character is not a slash, the string is interpreted literally as an attribute name. An attribute name
-- can contain any characters, but must not be empty.
--
-- If the first character is a slash, the string is interpreted as a slash-delimited path where the first path component
-- is an attribute name, and each subsequent path component is the name of a property in a JSON object. Any instances of
-- the characters "/" or "~" in a path component are escaped as "~1" or "~0" respectively. This syntax deliberately
-- resembles JSON Pointer, but no JSON Pointer behaviors other than those mentioned here are supported.
--
-- == Examples
--
-- Suppose there is a context whose JSON implementation looks like this:
--
--	{
--	  "kind": "user",
--	  "key": "value1",
--	  "address": {
--	    "street": {
--	      "line1": "value2",
--	      "line2": "value3"
--	    },
--	    "city": "value4"
--	  },
--	  "good/bad": "value5"
--	}
--
-- The attribute references "key" and "/key" would both point to "value1".
--
-- The attribute reference "/address/street/line1" would point to "value2".
--
-- The attribute references "good/bad" and "/good~1bad" would both point to
-- "value5".
--

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module LaunchDarkly.Server.Reference
  ( Reference
  , makeReference
  , makeLiteral
  , isValid
  , getError
  , getComponents
  , getRawPath
  )
where

import Data.Text (Text)
import qualified Data.Text as T

-- | data record for the Reference type.
data Reference =
  Valid { rawPath :: !Text, components :: ![Text] }
  | Invalid { rawPath :: !Text, error :: !Text }

-- | Creates a Reference from a string. For the supported syntax and examples, see comments on the
-- "LaunchDarkly.Server.Reference" module.
--
-- This function always returns a Reference that preserves the original string, even if validation fails, so that
-- accessing 'getRawPath' (or serializing the Reference to JSON) will produce the original string. If validation fails,
-- 'getError' will return an error and any SDK method that takes this Reference as a parameter will consider it
-- invalid.
makeReference :: Text -> Reference
makeReference "" = Invalid { rawPath = "", error = "empty reference" }
makeReference "/" = Invalid { rawPath = "/", error = "empty reference" }
makeReference value@(T.stripPrefix "/" -> Nothing) = Valid { rawPath = value, components = [value] }
makeReference value@(T.stripSuffix "/" -> Just _) = Invalid { rawPath = value, error = "trailing slash" }
makeReference value = foldr addComponentToReference (Valid { rawPath = value, components = [] }) (T.splitOn "/" $ T.drop 1 value)

-- | makeLiteral is similar to 'makeReference' except that it always interprets the string as a literal attribute name,
-- never as a slash-delimited path expression. There is no escaping or unescaping, even if the name contains literal '/'
-- or '~' characters. Since an attribute name can contain any characters, this method always returns a valid Reference
-- unless the name is empty.
--
-- For example: @makeLiteral "name"@ is exactly equivalent to @makeReference "name"@. @makeLiteral "a/b"@ is exactly
-- equivalent to @makeReference "a/b"@ (since the syntax used by 'makeReference' treats the whole string as a literal as
-- long as it does not start with a slash), or to @makeReference "/a~1b"@.
makeLiteral :: Text -> Reference
makeLiteral "" = Invalid { rawPath = "", error = "empty reference" }
makeLiteral value@(T.stripPrefix "/" -> Nothing) = Valid { rawPath = value, components = [value] }
makeLiteral value = Valid { rawPath = "/" <> (T.replace "/" "~1" $ T.replace "~" "~0" value), components = [value] }

-- | Returns True for a valid Reference; False otherwise.
--
-- A Reference is invalid if the input string is empty, or starts with a slash but is not a valid slash-delimited path,
-- or starts with a slash and contains an invalid escape sequence.
--
-- Otherwise, the Reference is valid, but that does not guarantee that such an attribute exists in any given Context.
-- For instance, @makeReference "name"@ is a valid Reference, but a specific Context might or might not have a name.
--
-- See comments on the "LaunchDarkly.Server.Reference" module for more details of the attribute reference syntax.
isValid :: Reference -> Bool
isValid (Invalid _ _) = False
isValid _ = True

-- | Returns 'Nothing' for a valid Reference, or a 'Just Text' error description for an invalid Reference.
--
-- See comments on the "LaunchDarkly.Server.Reference" module for more details of the attribute reference syntax.
getError :: Reference -> Maybe Text
getError (Invalid { error = e }) = Just e
getError _ = Nothing

-- | Retrieves path components from the attribute reference.
--
-- Invalid references will return an empty list.
--
-- > makeReference "" & getComponents     -- returns []
-- > makeReference "a" & getComponents    -- returns ["a"]
-- > makeReference "/a/b" & getComponents -- returns ["a", "b"]
getComponents :: Reference -> [Text]
getComponents (Valid { components }) = components
getComponents _ = []

-- | Returns the attribute reference as a string, in the same format provided to 'makeReference'.
--
-- If the Reference was created with 'makeReference', this value is identical to the original string. If it was created
-- with 'makeLiteral', the value may be different due to unescaping (for instance, an attribute whose name is "/a" would
-- be represented as "~1a").
getRawPath :: Reference -> Text
getRawPath = rawPath

-- Method intended to be used with a foldr. If you do not use this with a foldr, the components will be in the wrong
-- order as this method does prepending.
--
-- This function helps assist in the construction of a Valid reference by incrementally adding a new component to the
-- Reference. If the component cannot be added, or if the Reference is already invalid, we return an Invalid reference
-- with the appropriate error description.
addComponentToReference :: Text -> Reference -> Reference
addComponentToReference _ r@(Invalid _ _) = r
addComponentToReference "" (Valid { rawPath }) = Invalid { rawPath, error = "double slash" }
addComponentToReference component (Valid { rawPath, components }) = case unescapePath component of
  Left c -> Valid { rawPath, components = (c:components) }
  Right e -> Invalid { rawPath, error = e }

-- Performs unescaping of attribute reference path components:
--
-- "~1" becomes "/"
-- "~0" becomes "~"
-- "~" followed by any character other than "0" or "1" is invalid
--
-- This method returns an Either. Left Text is the path if unescaping was valid; otherwise, Right Text will be a
-- description error message.
unescapePath :: Text -> Either Text Text
unescapePath value@(T.isInfixOf "~" -> False) = Left value
unescapePath (T.stripSuffix "~" -> Just _) = Right "invalid escape sequence"
unescapePath value =
  let component = T.foldl unescapeComponent (ComponentState { acc = [], valid = True, inEscape = False }) value
  in case component of
    ComponentState { acc = acc, valid = True } -> Left $ T.pack $ reverse acc
    _ -> Right "invalid escape sequence"

-- Component state is a helper record to assist with unescaping a string.
--
-- When we are processing a string, we have to ensure that ~ is followed by 0 or 1. Any other value is invalid. To track
-- this, we update this component state through a fold operation.
data ComponentState = ComponentState
  { acc :: ![Char] -- Container to hold the piece of the input that has been successfully parsed.
  , valid :: !Bool -- Is the state currently valid?
  , inEscape :: !Bool -- Was the last character seen a tilde?
  }

-- Intended to be used in a foldl operation to apply unescaping rules as defined in 'unescapePath'.
--
-- Note that the 'ComponentState.acc' will be built backwards. This is because prepending is faster in Haskell.
-- Calling functions should reverse accordingly.
unescapeComponent :: ComponentState -> Char -> ComponentState
-- Short circuit if we are already invalid
unescapeComponent component@(ComponentState { valid = False }) _ = component
-- Escape mode with a 0 or 1 means a valid escape sequence. We can append this to the state's accumulator.
unescapeComponent component@(ComponentState { acc, inEscape = True }) '0' = component { acc = '~':acc, valid = True, inEscape = False }
unescapeComponent component@(ComponentState { acc, inEscape = True }) '1' = component { acc = '/':acc, valid = True, inEscape = False }
-- Any other character during an escape sequence isn't valid
unescapeComponent component@(ComponentState { inEscape = True }) _ = component { valid = False }
-- ~ means we should start escaping
unescapeComponent component '~' = component { inEscape = True }
-- Regular characters can be added without issue
unescapeComponent component@(ComponentState { acc }) c = component { acc = c:acc }