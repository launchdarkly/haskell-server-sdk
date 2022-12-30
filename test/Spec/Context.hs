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
    assertEqual "" False $ contextIsValid context
    assertEqual "" (Just expectedError) $ contextError context)

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

singleContextSupportsValueRetrieval :: Test
singleContextSupportsValueRetrieval = TestCase $
  let address = Object $ fromList [("city", "Chicago"), ("state", "IL")]
      favorites = Object $ fromList [("food", "Pizza"), ("sport", "baseball")]
      preferences = Object $ fromList [("favorites", favorites)]
      user = makeContext "user-key" "user"
        & contextSetName "Example"
        & contextSetAnonymous False
        & contextSetAttribute "groups" (Array $ V.fromList ["beta_testers"])
        & contextSetAttribute "address" address
        & contextSetAttribute "preferences" preferences
  in (do
    assertEqual "" "user-key" $ getValueForReference (R.makeReference "key") user
    assertEqual "" "user" $ getValueForReference (R.makeReference "kind") user
    assertEqual "" "Example" $ getValueForReference (R.makeReference "name") user
    assertEqual "" (Bool False) $ getValueForReference (R.makeReference "anonymous") user
    assertEqual "" "Chicago" $ getValueForReference (R.makeReference "/address/city") user
    assertEqual "" "baseball" $ getValueForReference (R.makeReference "/preferences/favorites/sport") user
    assertEqual "" (Array $ V.fromList ["beta_testers"]) $ getValueForReference (R.makeReference "/groups") user
    assertEqual "" Null $ getValueForReference (R.makeReference "/groups/0") user)

multiKindCanOnlyRetrieveKindAttribute :: Test
multiKindCanOnlyRetrieveKindAttribute = TestCase $
  let user = makeContext "user-key" "user"
      org = makeContext "org-key" "org"
      multi = makeMultiContext [user, org]
  in (do
    assertEqual "" "multi" $ getValueForReference (R.makeReference "kind") multi
    assertEqual "" Null $ getValueForReference (R.makeReference "key") multi)

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
  ]