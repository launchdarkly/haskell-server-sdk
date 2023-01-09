module Spec.Context (allTests) where

import Test.HUnit

import LaunchDarkly.Server.Context
import qualified LaunchDarkly.Server.Reference as R
import Control.Monad.Cont (liftIO)
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Aeson (Value(..))
import Data.Function ((&))
import LaunchDarkly.AesonCompat (fromList)

confirmInvalidContext :: Context -> Text -> Assertion
confirmInvalidContext context expectedError = liftIO $ (do
    assertEqual "" False $ isValid context
    assertEqual "" (Just expectedError) $ getError context)

invalidKey :: Test
invalidKey = TestCase $
  confirmInvalidContext (makeContext "" "user") "context key must not be empty"

invalidKinds :: Test
invalidKinds = TestCase $ (do
  confirmInvalidContext (makeContext "user-key" "") "context kind must not be empty"
  confirmInvalidContext (makeContext "user-key" "kind") "context kind cannot be 'kind'"
  confirmInvalidContext (makeContext "user-key" "multi") "context kind cannot be 'multi'"
  confirmInvalidContext (makeContext "user-key" "invalid*characters") "context kind contains disallowed characters"
  confirmInvalidContext (makeContext "user-key" "NO-YELLING") "context kind contains disallowed characters")

multiKindRequiresOne :: Test
multiKindRequiresOne = TestCase $
  confirmInvalidContext (makeMultiContext []) "multi-kind contexts require at least one single-kind context"

multiKindRequiresUnique :: Test
multiKindRequiresUnique = TestCase $
  confirmInvalidContext (makeMultiContext [user1, user2]) "multi-kind contexts cannot contain two single-kind contexts with the same kind"
  where user1 = makeContext "user-key1" "user"
        user2 = makeContext "user-key2" "user"

multiKindRequiresSingleContextsOnly :: Test
multiKindRequiresSingleContextsOnly = TestCase $
  confirmInvalidContext (makeMultiContext [user, org, multi]) "multi-kind contexts can only contain single-kind contexts"
  where user = makeContext "user-key" "user"
        org = makeContext "org-key" "org"
        multi = makeMultiContext [user, org]

multiKindWithSingleKindWillReturnSingleKind :: Test
multiKindWithSingleKindWillReturnSingleKind = TestCase $
  assertEqual "" user (makeMultiContext [user])
  where user = makeContext "user-key" "user"

multiKindCanOnlyRetrieveKindAttribute :: Test
multiKindCanOnlyRetrieveKindAttribute = TestCase $
  let user = makeContext "user-key" "user"
      org = makeContext "org-key" "org"
      multi = makeMultiContext [user, org]
  in (do
    assertEqual "" "multi" $ getValue "kind" multi
    assertEqual "" Null $ getValue "key" multi

    assertEqual "" "multi" $ getValueForReference (R.makeReference "kind") multi
    assertEqual "" Null $ getValueForReference (R.makeReference "key") multi)

singleContextSupportsValueRetrieval :: Test
singleContextSupportsValueRetrieval = TestCase $
  let address = Object $ fromList [("city", "Chicago"), ("state", "IL")]
      favorites = Object $ fromList [("food", "Pizza"), ("sport", "baseball")]
      preferences = Object $ fromList [("favorites", favorites)]
      user = makeContext "user-key" "user"
        & withName "Example"
        & withAnonymous False
        & withAttribute "groups" (Array $ V.fromList ["beta_testers"])
        & withAttribute "address" address
        & withAttribute "preferences" preferences
        & withAttribute "complex/and-weird~attribute" "nailed it"
  in (do
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
    assertEqual "" (Array $ V.fromList ["beta_testers"]) $ getValueForReference (R.makeReference "/groups") user
    assertEqual "" Null $ getValueForReference (R.makeReference "/groups/0") user
    assertEqual "" "nailed it" $ getValueForReference (R.makeReference "/complex~1and-weird~0attribute") user)

invalidKindCannotRetrieveAnything :: Test
invalidKindCannotRetrieveAnything = TestCase $
  let invalid = makeContext "user-key" "multi" & withName "Sandy" & withAttribute "nickname" "Sam"
  in (do
    assertEqual "" Null $ getValue "kind" invalid
    assertEqual "" Null $ getValue "key" invalid
    assertEqual "" Null $ getValue "name" invalid
    assertEqual "" Null $ getValue "nickname" invalid

    assertEqual "" Null $ getValueForReference (R.makeReference "kind") invalid
    assertEqual "" Null $ getValueForReference (R.makeReference "key") invalid
    assertEqual "" Null $ getValueForReference (R.makeReference "name") invalid
    assertEqual "" Null $ getValueForReference (R.makeReference "nickname") invalid)

setAndVerifyAttribute :: Text -> Value -> Value -> Context -> Assertion
setAndVerifyAttribute attribute attempted expected context =
  assertEqual "" expected (withAttribute attribute attempted context & getValue attribute)

cannotUseWithAttributeToSetRestrictedAttributes :: Test
cannotUseWithAttributeToSetRestrictedAttributes = TestCase $
  let user = makeContext "user-key" "user"
        & withName "Sandy"
        & withAnonymous True
        & withAttribute "testing" "something"
      invalid = makeContext "invalid-key" "kind"
      multi = makeMultiContext [makeContext "org-key" "org", user]
  in (do
    setAndVerifyAttribute "kind" "org" "user" user
    setAndVerifyAttribute "key" "new-key" "user-key" user
    setAndVerifyAttribute "name" "Jim" "Jim" user
    setAndVerifyAttribute "name" (Bool True) "Sandy" user
    setAndVerifyAttribute "anonymous" (Bool False) (Bool False) user
    setAndVerifyAttribute "anonymous" "false" (Bool True) user
    setAndVerifyAttribute "_meta" "anything" Null user
    setAndVerifyAttribute "privateAttributeNames" (Array $ V.fromList ["name"]) Null user

    setAndVerifyAttribute "kind" "org" "multi" multi
    setAndVerifyAttribute "key" "new-key" Null multi
    setAndVerifyAttribute "name" "Jim" Null multi
    setAndVerifyAttribute "name" (Bool True) Null multi
    setAndVerifyAttribute "anonymous" (Bool False) Null multi
    setAndVerifyAttribute "anonymous" "false" Null multi
    setAndVerifyAttribute "_meta" "anything" Null multi
    setAndVerifyAttribute "privateAttributeNames" (Array $ V.fromList ["name"]) Null multi

    setAndVerifyAttribute "kind" "org" Null invalid
    setAndVerifyAttribute "key" "new-key" Null invalid
    setAndVerifyAttribute "name" "Jim" Null invalid
    setAndVerifyAttribute "name" (Bool True) Null invalid
    setAndVerifyAttribute "anonymous" (Bool False) Null invalid
    setAndVerifyAttribute "anonymous" "false" Null invalid
    setAndVerifyAttribute "_meta" "anything" Null invalid
    setAndVerifyAttribute "privateAttributeNames" (Array $ V.fromList ["name"]) Null invalid)

allTests :: Test
allTests = TestList
  [ invalidKey
  , invalidKinds
  , multiKindRequiresOne
  , multiKindRequiresUnique
  , multiKindRequiresSingleContextsOnly
  , multiKindWithSingleKindWillReturnSingleKind
  , multiKindCanOnlyRetrieveKindAttribute
  , singleContextSupportsValueRetrieval
  , invalidKindCannotRetrieveAnything
  , cannotUseWithAttributeToSetRestrictedAttributes
  ]
