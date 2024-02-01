module Spec.Context (allTests) where

import Test.HUnit

import Control.Monad.Cont (liftIO)
import Data.Aeson (Value (..), decode, encode)
import Data.Function ((&))
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Data.Text (Text)
import GHC.Exts (fromList)
import LaunchDarkly.AesonCompat (lookupKey)
import LaunchDarkly.Server.Config (configSetAllAttributesPrivate, makeConfig)
import LaunchDarkly.Server.Context
import LaunchDarkly.Server.Context.Internal (redactContext)
import qualified LaunchDarkly.Server.Reference as R

confirmInvalidContext :: Context -> Text -> Assertion
confirmInvalidContext context expectedError =
    liftIO $
        ( do
            assertEqual "" False $ isValid context
            assertEqual "" expectedError $ getError context
        )

invalidKey :: Test
invalidKey =
    TestCase $
        confirmInvalidContext (makeContext "" "user") "context key must not be empty"

invalidKinds :: Test
invalidKinds =
    TestCase $
        ( do
            confirmInvalidContext (makeContext "user-key" "") "context kind must not be empty"
            confirmInvalidContext (makeContext "user-key" "kind") "context kind cannot be 'kind'"
            confirmInvalidContext (makeContext "user-key" "multi") "context kind cannot be 'multi'"
            confirmInvalidContext (makeContext "user-key" "invalid*characters") "context kind contains disallowed characters"
        )

multiKindRequiresOne :: Test
multiKindRequiresOne =
    TestCase $
        confirmInvalidContext (makeMultiContext []) "multi-kind contexts require at least one single-kind context"

multiKindRequiresUnique :: Test
multiKindRequiresUnique =
    TestCase $
        confirmInvalidContext (makeMultiContext [user1, user2]) "multi-kind contexts cannot contain two single-kind contexts with the same kind"
  where
    user1 = makeContext "user-key1" "user"
    user2 = makeContext "user-key2" "user"

multiKindRequiresSingleContextsOnly :: Test
multiKindRequiresSingleContextsOnly =
    TestCase $
        confirmInvalidContext (makeMultiContext [user, org, multi]) "multi-kind contexts can only contain single-kind contexts"
  where
    user = makeContext "user-key" "user"
    org = makeContext "org-key" "org"
    multi = makeMultiContext [user, org]

multiKindWithSingleKindWillReturnSingleKind :: Test
multiKindWithSingleKindWillReturnSingleKind =
    TestCase $
        assertEqual "" user (makeMultiContext [user])
  where
    user = makeContext "user-key" "user"

multiKindCanOnlyRetrieveKindAttribute :: Test
multiKindCanOnlyRetrieveKindAttribute =
    TestCase $
        let user = makeContext "user-key" "user"
            org = makeContext "org-key" "org"
            multi = makeMultiContext [user, org]
         in ( do
                assertEqual "" "multi" $ getValue "kind" multi
                assertEqual "" Null $ getValue "key" multi

                assertEqual "" "multi" $ getValueForReference (R.makeReference "kind") multi
                assertEqual "" Null $ getValueForReference (R.makeReference "key") multi
            )

canRetrievalIndividualContextsFromMultiKindContext :: Test
canRetrievalIndividualContextsFromMultiKindContext =
    TestCase $
        let user = makeContext "user-key" "user"
            org = makeContext "org-key" "org"
            multi = makeMultiContext [user, org]
         in ( do
                assertEqual "" (Just user) $ getIndividualContext "user" multi
                assertEqual "" (Just org) $ getIndividualContext "org" multi
                assertEqual "" Nothing $ getIndividualContext "device" multi
            )

canRetrievalIndividualContextsFromSingleKindContext :: Test
canRetrievalIndividualContextsFromSingleKindContext =
    TestCase $
        let context = makeContext "user-key" "user"
         in ( do
                assertEqual "" (Just context) $ getIndividualContext "user" context
                assertEqual "" Nothing $ getIndividualContext "org" context
            )

singleContextSupportsValueRetrieval :: Test
singleContextSupportsValueRetrieval =
    TestCase $
        let address = Object $ fromList [("city", "Chicago"), ("state", "IL")]
            favorites = Object $ fromList [("food", "Pizza"), ("sport", "baseball")]
            preferences = Object $ fromList [("favorites", favorites)]
            user =
                makeContext "user-key" "user"
                    & withName "Example"
                    & withAnonymous False
                    & withAttribute "groups" (Array $ fromList ["beta_testers"])
                    & withAttribute "address" address
                    & withAttribute "preferences" preferences
                    & withAttribute "complex/and-weird~attribute" "nailed it"
         in ( do
                assertEqual "" "user-key" $ getValue "key" user
                assertEqual "" "user" $ getValue "kind" user
                assertEqual "" "Example" $ getValue "name" user
                assertEqual "" (Bool False) $ getValue "anonymous" user
                assertEqual "" Null $ getValue "/address/city" user
                assertEqual "" Null $ getValue "/preferences/favorites/sport" user
                assertEqual "" Null $ getValue "/groups" user
                assertEqual "" Null $ getValue "/groups/0" user
                assertEqual "" "nailed it" $ getValue "complex/and-weird~attribute" user

                assertEqual "" "user-key" $ getValueForReference (R.makeReference "key") user
                assertEqual "" "user" $ getValueForReference (R.makeReference "kind") user
                assertEqual "" "Example" $ getValueForReference (R.makeReference "name") user
                assertEqual "" (Bool False) $ getValueForReference (R.makeReference "anonymous") user
                assertEqual "" "Chicago" $ getValueForReference (R.makeReference "/address/city") user
                assertEqual "" "baseball" $ getValueForReference (R.makeReference "/preferences/favorites/sport") user
                assertEqual "" (Array $ fromList ["beta_testers"]) $ getValueForReference (R.makeReference "/groups") user
                assertEqual "" Null $ getValueForReference (R.makeReference "/groups/0") user
                assertEqual "" "nailed it" $ getValueForReference (R.makeReference "/complex~1and-weird~0attribute") user
            )

invalidKindCannotRetrieveAnything :: Test
invalidKindCannotRetrieveAnything =
    TestCase $
        let invalid = makeContext "user-key" "multi" & withName "Sandy" & withAttribute "nickname" "Sam"
         in ( do
                assertEqual "" Null $ getValue "kind" invalid
                assertEqual "" Null $ getValue "key" invalid
                assertEqual "" Null $ getValue "name" invalid
                assertEqual "" Null $ getValue "nickname" invalid

                assertEqual "" Null $ getValueForReference (R.makeReference "kind") invalid
                assertEqual "" Null $ getValueForReference (R.makeReference "key") invalid
                assertEqual "" Null $ getValueForReference (R.makeReference "name") invalid
                assertEqual "" Null $ getValueForReference (R.makeReference "nickname") invalid
            )

setAndVerifyAttribute :: Text -> Value -> Value -> Context -> Assertion
setAndVerifyAttribute attribute attempted expected context =
    assertEqual "" expected (withAttribute attribute attempted context & getValue attribute)

cannotUseWithAttributeToSetRestrictedAttributes :: Test
cannotUseWithAttributeToSetRestrictedAttributes =
    TestCase $
        let user =
                makeContext "user-key" "user"
                    & withName "Sandy"
                    & withAnonymous True
                    & withAttribute "testing" "something"
            invalid = makeContext "invalid-key" "kind"
            multi = makeMultiContext [makeContext "org-key" "org", user]
         in ( do
                setAndVerifyAttribute "kind" "org" "user" user
                setAndVerifyAttribute "key" "new-key" "user-key" user
                setAndVerifyAttribute "name" "Jim" "Jim" user
                setAndVerifyAttribute "name" (Bool True) "Sandy" user
                setAndVerifyAttribute "anonymous" (Bool False) (Bool False) user
                setAndVerifyAttribute "anonymous" "false" (Bool True) user
                setAndVerifyAttribute "_meta" "anything" Null user
                setAndVerifyAttribute "privateAttributeNames" (Array $ fromList ["name"]) Null user

                setAndVerifyAttribute "kind" "org" "multi" multi
                setAndVerifyAttribute "key" "new-key" Null multi
                setAndVerifyAttribute "name" "Jim" Null multi
                setAndVerifyAttribute "name" (Bool True) Null multi
                setAndVerifyAttribute "anonymous" (Bool False) Null multi
                setAndVerifyAttribute "anonymous" "false" Null multi
                setAndVerifyAttribute "_meta" "anything" Null multi
                setAndVerifyAttribute "privateAttributeNames" (Array $ fromList ["name"]) Null multi

                setAndVerifyAttribute "kind" "org" Null invalid
                setAndVerifyAttribute "key" "new-key" Null invalid
                setAndVerifyAttribute "name" "Jim" Null invalid
                setAndVerifyAttribute "name" (Bool True) Null invalid
                setAndVerifyAttribute "anonymous" (Bool False) Null invalid
                setAndVerifyAttribute "anonymous" "false" Null invalid
                setAndVerifyAttribute "_meta" "anything" Null invalid
                setAndVerifyAttribute "privateAttributeNames" (Array $ fromList ["name"]) Null invalid
            )

canParseFromLegacyUserFormat :: Test
canParseFromLegacyUserFormat =
    TestCase $
        let jsonString = "{\"key\": \"user-key\", \"ip\": \"127.0.0.1\", \"custom\": {\"address\": {\"street\": \"123 Easy St\", \"city\": \"Anytown\"}, \"language\": \"Haskell\"}}"
            context :: Context = fromJust $ decode jsonString
         in ( do
                assertBool "" $ isValid context
                assertEqual "" "user" $ getValue "kind" context
                assertEqual "" "user-key" $ getValue "key" context
                assertEqual "" "127.0.0.1" $ getValue "ip" context
                assertEqual "" "Haskell" $ getValue "language" context
                assertEqual "" "123 Easy St" $ getValueForReference (R.makeReference "/address/street") context
                assertEqual "" "Anytown" $ getValueForReference (R.makeReference "/address/city") context
            )

canParseSingleKindFormat :: Test
canParseSingleKindFormat =
    TestCase $
        let jsonString = "{\"key\": \"org-key\", \"kind\": \"org\", \"ip\": \"127.0.0.1\", \"custom\": {\"address\": {\"street\": \"123 Easy St\", \"city\": \"Anytown\"}, \"language\": \"Haskell\"}}"
            context :: Context = fromJust $ decode jsonString
         in ( do
                assertBool "" $ isValid context
                assertEqual "" "org" $ getValue "kind" context
                assertEqual "" "org-key" $ getValue "key" context
                assertEqual "" "127.0.0.1" $ getValue "ip" context
                assertEqual "" Null $ getValue "language" context
                assertEqual "" "Haskell" $ getValueForReference (R.makeReference "/custom/language") context
                assertEqual "" Null $ getValueForReference (R.makeReference "/address/street") context
                assertEqual "" Null $ getValueForReference (R.makeReference "/address/city") context
                assertEqual "" "123 Easy St" $ getValueForReference (R.makeReference "/custom/address/street") context
                assertEqual "" "Anytown" $ getValueForReference (R.makeReference "/custom/address/city") context
            )

canParseMultiKindFormat :: Test
canParseMultiKindFormat =
    TestCase $
        let jsonString = "{\"kind\": \"multi\", \"user\": {\"key\": \"user-key\", \"name\": \"Sandy\"}, \"org\": {\"key\": \"org-key\", \"name\": \"LaunchDarkly\"}}"
            context :: Context = fromJust $ decode jsonString
            userContext = fromJust $ getIndividualContext "user" context
            orgContext = fromJust $ getIndividualContext "org" context
         in ( do
                assertBool "" $ isValid context
                assertEqual "" "multi" $ getValue "kind" context

                assertEqual "" "user" $ getValue "kind" userContext
                assertEqual "" "user-key" $ getValue "key" userContext
                assertEqual "" "Sandy" $ getValue "name" userContext

                assertEqual "" "org" $ getValue "kind" orgContext
                assertEqual "" "org-key" $ getValue "key" orgContext
                assertEqual "" "LaunchDarkly" $ getValue "name" orgContext
            )

canRedactAttributesCorrectly :: Test
canRedactAttributesCorrectly = TestCase $ do
    assertEqual "" expectedRedacted (fromJust $ lookupKey "redactedAttributes" meta)
    assertEqual "" "user" (fromJust $ lookupKey "kind" decodedIntoMap)
    assertEqual "" "user-key" (fromJust $ lookupKey "key" decodedIntoMap)
    assertEqual "" "Sandy" (fromJust $ lookupKey "firstName" decodedIntoMap)
    assertEqual "" "Beaches" (fromJust $ lookupKey "lastName" decodedIntoMap)
    assertEqual "" hobbies (fromJust $ lookupKey "hobbies" decodedIntoMap)
    assertEqual "" expectedAddress (fromJust $ lookupKey "address" decodedIntoMap)
  where
    config = makeConfig "sdk-key"

    address = Object $ fromList [("city", "Chicago"), ("state", "IL")]
    hobbies = (Array $ fromList ["coding", "reading"])

    context =
        makeContext "user-key" "user"
            & withAttribute "name" "Sandy"
            & withAttribute "firstName" "Sandy"
            & withAttribute "lastName" "Beaches"
            & withAttribute "address" address
            & withAttribute "hobbies" hobbies
            & withPrivateAttributes (S.fromList [R.makeLiteral "key", R.makeLiteral "kind", R.makeLiteral "anonymous", R.makeLiteral "name", R.makeReference "/address/city", R.makeReference "/hobbies/0"])

    jsonByteString = encode $ redactContext config context
    decodedAsValue = fromJust $ decode jsonByteString :: Value
    decodedIntoMap = case decodedAsValue of (Object o) -> o; _ -> error "expected object"
    meta = case lookupKey "_meta" decodedIntoMap of (Just (Object o)) -> o; _ -> error "expected object"
    expectedRedacted = Array $ fromList ["/address/city", "name"]
    expectedAddress = Object $ fromList [("state", "IL")]

canRedactAllAttributesCorrectly :: Test
canRedactAllAttributesCorrectly = TestCase $ do
    assertEqual "" expectedRedacted (fromJust $ lookupKey "redactedAttributes" meta)
    assertEqual "" "user" (fromJust $ lookupKey "kind" decodedIntoMap)
    assertEqual "" "user-key" (fromJust $ lookupKey "key" decodedIntoMap)
    assertEqual "" Nothing (lookupKey "firstName" decodedIntoMap)
    assertEqual "" Nothing (lookupKey "lastName" decodedIntoMap)
    assertEqual "" Nothing (lookupKey "hobbies" decodedIntoMap)
    assertEqual "" Nothing (lookupKey "address" decodedIntoMap)
  where
    config = makeConfig "sdk-key" & configSetAllAttributesPrivate True

    address = Object $ fromList [("city", "Chicago"), ("state", "IL")]

    context =
        makeContext "user-key" "user"
            & withAttribute "name" "Sandy"
            & withAttribute "firstName" "Sandy"
            & withAttribute "lastName" "Beaches"
            & withAttribute "address" address
            & withAttribute "hobbies" (Array $ fromList ["coding", "reading"])

    jsonByteString = encode $ redactContext config context
    decodedAsValue = fromJust $ decode jsonByteString :: Value
    decodedIntoMap = case decodedAsValue of (Object o) -> o; _ -> error "expected object"
    meta = case lookupKey "_meta" decodedIntoMap of (Just (Object o)) -> o; _ -> error "expected object"
    expectedRedacted = Array $ fromList ["address", "firstName", "hobbies", "lastName", "name"]
    expectedAddress = Object $ fromList [("state", "IL")]

allTests :: Test
allTests =
    TestList
        [ invalidKey
        , invalidKinds
        , multiKindRequiresOne
        , multiKindRequiresUnique
        , multiKindRequiresSingleContextsOnly
        , multiKindWithSingleKindWillReturnSingleKind
        , multiKindCanOnlyRetrieveKindAttribute
        , canRetrievalIndividualContextsFromMultiKindContext
        , canRetrievalIndividualContextsFromSingleKindContext
        , singleContextSupportsValueRetrieval
        , invalidKindCannotRetrieveAnything
        , cannotUseWithAttributeToSetRestrictedAttributes
        , canParseFromLegacyUserFormat
        , canParseSingleKindFormat
        , canParseMultiKindFormat
        , canRedactAttributesCorrectly
        , canRedactAllAttributesCorrectly
        ]
