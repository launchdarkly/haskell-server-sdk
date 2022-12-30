module Spec.Context (allTests) where

import Test.HUnit

import LaunchDarkly.Server.Context
import Control.Monad.Cont (liftIO)
import Data.Text (Text)

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

allTests :: Test
allTests = TestList
  [ invalidKey
  , invalidKinds
  , multiKindRequiresOne
  , multiKindRequiresUnique
  , multiKindRequiresSingleContextsOnly
  , multiKindWithSingleKindWillReturnSingleKind
  ]