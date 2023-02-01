module Spec.Segment (allTests) where

import Data.Aeson.Types (Value (..))
import Data.Function ((&))
import Data.Generics.Product (getField)
import qualified Data.HashSet as HS
import Test.HUnit
import Util.Features

import LaunchDarkly.Server.Client
import LaunchDarkly.Server.Client.Internal
import LaunchDarkly.Server.Config
import LaunchDarkly.Server.Context (makeContext, withAttribute)
import LaunchDarkly.Server.Evaluate
import LaunchDarkly.Server.Features
import LaunchDarkly.Server.Operators
import LaunchDarkly.Server.Reference (makeLiteral)
import LaunchDarkly.Server.Store
import LaunchDarkly.Server.Store.Internal
import LaunchDarkly.Server.User
import LaunchDarkly.Server.User.Internal

makeEmptyStore = do
    handle <- makeStoreIO Nothing 0
    initializeStore handle mempty mempty
    pure handle

makeTestClient :: IO Client
makeTestClient = do
    (Client client) <- makeClient $ (makeConfig "") & configSetOffline True
    initializeStore (getField @"store" client) mempty mempty
    pure (Client client)

testExplicitIncludeUser :: Test
testExplicitIncludeUser = TestCase $ do
    store <- makeEmptyStore
    segmentContainsContext store segment user HS.empty >>= assertEqual "" (Right True)
    segmentContainsContext store segment org HS.empty >>= assertEqual "" (Right False)
  where
    segment =
        Segment
            { key = "test"
            , included = HS.fromList ["foo"]
            , includedContexts = mempty
            , excluded = HS.empty
            , excludedContexts = mempty
            , salt = "abcdef"
            , rules = []
            , version = 1
            , deleted = False
            }

    user = makeContext "foo" "user"
    org = makeContext "foo" "org"

testExplicitIncludeContextKind :: Test
testExplicitIncludeContextKind = TestCase $ do
    store <- makeEmptyStore
    segmentContainsContext store segment user HS.empty >>= assertEqual "" (Right True)
    segmentContainsContext store segment org HS.empty >>= assertEqual "" (Right False)
  where
    segment =
        Segment
            { key = "test"
            , included = HS.empty
            , includedContexts = [SegmentTarget {values = HS.fromList ["foo"], contextKind = "user"}]
            , excluded = HS.empty
            , excludedContexts = mempty
            , salt = "abcdef"
            , rules = []
            , version = 1
            , deleted = False
            }

    user = makeContext "foo" "user"
    org = makeContext "foo" "org"

testExplicitExcludeUser :: Test
testExplicitExcludeUser = TestCase $ do
    store <- makeEmptyStore
    segmentContainsContext store segment user HS.empty >>= assertEqual "" (Right False)
  where
    segment =
        Segment
            { key = "test"
            , included = HS.empty
            , includedContexts = mempty
            , excluded = HS.fromList ["foo"]
            , excludedContexts = mempty
            , salt = "abcdef"
            , rules = []
            , version = 1
            , deleted = False
            }

    user = makeContext "foo" "user"

testExplicitExcludeContextKind :: Test
testExplicitExcludeContextKind = TestCase $ do
    store <- makeEmptyStore
    segmentContainsContext store segment user HS.empty >>= assertEqual "" (Right False)
  where
    segment =
        Segment
            { key = "test"
            , included = HS.empty
            , includedContexts = mempty
            , excluded = HS.empty
            , excludedContexts = [SegmentTarget {values = HS.fromList ["foo"], contextKind = "user"}]
            , salt = "abcdef"
            , rules = []
            , version = 1
            , deleted = False
            }

    user = makeContext "foo" "user"

testExplicitIncludeHasPrecedence :: Test
testExplicitIncludeHasPrecedence = TestCase $ do
    store <- makeEmptyStore
    segmentContainsContext store segment user HS.empty >>= assertEqual "" (Right True)
  where
    segment =
        Segment
            { key = "test"
            , included = HS.fromList ["foo"]
            , includedContexts = mempty
            , excluded = HS.fromList ["foo"]
            , excludedContexts = mempty
            , salt = "abcdef"
            , rules = []
            , version = 1
            , deleted = False
            }

    user = makeContext "foo" "user"

testExplicitIncludeContextsHasPrecedence :: Test
testExplicitIncludeContextsHasPrecedence = TestCase $ do
    store <- makeEmptyStore
    segmentContainsContext store segment user HS.empty >>= assertEqual "" (Right True)
  where
    segment =
        Segment
            { key = "test"
            , included = HS.empty
            , includedContexts = [SegmentTarget {values = HS.fromList ["foo"], contextKind = "user"}]
            , excluded = HS.fromList ["foo"]
            , excludedContexts = [SegmentTarget {values = HS.fromList ["foo"], contextKind = "user"}]
            , salt = "abcdef"
            , rules = []
            , version = 1
            , deleted = False
            }

    user = makeContext "foo" "user"

testNeitherIncludedNorExcluded :: Test
testNeitherIncludedNorExcluded = TestCase $ do
    store <- makeEmptyStore
    segmentContainsContext store segment user HS.empty >>= assertEqual "" (Right False)
  where
    segment =
        Segment
            { key = "test"
            , included = HS.fromList [""]
            , includedContexts = mempty
            , excluded = HS.fromList [""]
            , excludedContexts = mempty
            , salt = "abcdef"
            , rules = []
            , version = 1
            , deleted = False
            }

    user = makeContext "foo" "user"

testMatchingRuleWithFullRollout :: Test
testMatchingRuleWithFullRollout = TestCase $ do
    store <- makeEmptyStore
    segmentContainsContext store segment context HS.empty >>= assertEqual "" (Right True)
  where
    segment =
        Segment
            { key = "test"
            , included = HS.empty
            , includedContexts = mempty
            , excluded = HS.empty
            , excludedContexts = mempty
            , salt = "abcdef"
            , rules =
                [ SegmentRule
                    { id = "rule"
                    , clauses =
                        [ Clause
                            { attribute = makeLiteral "email"
                            , contextKind = "user"
                            , negate = False
                            , op = OpIn
                            , values = [String "test@example.com"]
                            }
                        ]
                    , weight = Just 100000
                    , bucketBy = Nothing
                    , rolloutContextKind = Just "user"
                    }
                ]
            , version = 1
            , deleted = False
            }

    context = makeContext "foo" "user" & withAttribute "email" "test@example.com"

testMatchingRuleWithZeroRollout :: Test
testMatchingRuleWithZeroRollout = TestCase $ do
    store <- makeEmptyStore
    segmentContainsContext store segment context HS.empty >>= assertEqual "" (Right False)
  where
    segment =
        Segment
            { key = "test"
            , included = HS.empty
            , includedContexts = mempty
            , excluded = HS.empty
            , excludedContexts = mempty
            , salt = "abcdef"
            , rules =
                [ SegmentRule
                    { id = "rule"
                    , clauses =
                        [ Clause
                            { attribute = makeLiteral "email"
                            , contextKind = "user"
                            , negate = False
                            , op = OpIn
                            , values = [String "test@example.com"]
                            }
                        ]
                    , weight = Just 0
                    , bucketBy = Nothing
                    , rolloutContextKind = Just "user"
                    }
                ]
            , version = 1
            , deleted = False
            }

    context = makeContext "foo" "user" & withAttribute "email" "test@example.com"

testMatchingRuleWithMultipleClauses :: Test
testMatchingRuleWithMultipleClauses = TestCase $ do
    store <- makeEmptyStore
    segmentContainsContext store segment context HS.empty >>= assertEqual "" (Right True)
  where
    segment =
        Segment
            { key = "test"
            , included = HS.empty
            , includedContexts = mempty
            , excluded = HS.empty
            , excludedContexts = mempty
            , salt = "abcdef"
            , rules =
                [ SegmentRule
                    { id = "rule"
                    , clauses =
                        [ Clause
                            { attribute = makeLiteral "email"
                            , contextKind = "user"
                            , negate = False
                            , op = OpIn
                            , values = [String "test@example.com"]
                            }
                        , Clause
                            { attribute = makeLiteral "name"
                            , contextKind = "user"
                            , negate = False
                            , op = OpIn
                            , values = ["bob"]
                            }
                        ]
                    , weight = Nothing
                    , bucketBy = Nothing
                    , rolloutContextKind = Just "user"
                    }
                ]
            , version = 1
            , deleted = False
            }

    context =
        makeContext "foo" "user"
            & withAttribute "email" "test@example.com"
            & withAttribute "name" "bob"

testNonMatchingRuleWithMultipleClauses :: Test
testNonMatchingRuleWithMultipleClauses = TestCase $ do
    store <- makeEmptyStore
    segmentContainsContext store segment context HS.empty >>= assertEqual "" (Right False)
  where
    segment =
        Segment
            { key = "test"
            , included = HS.empty
            , includedContexts = mempty
            , excluded = HS.empty
            , excludedContexts = mempty
            , salt = "abcdef"
            , rules =
                [ SegmentRule
                    { id = "rule"
                    , clauses =
                        [ Clause
                            { attribute = makeLiteral "email"
                            , contextKind = "user"
                            , negate = False
                            , op = OpIn
                            , values = [String "test@example.com"]
                            }
                        , Clause
                            { attribute = makeLiteral "name"
                            , contextKind = "user"
                            , negate = False
                            , op = OpIn
                            , values = ["bill"]
                            }
                        ]
                    , weight = Nothing
                    , bucketBy = Nothing
                    , rolloutContextKind = Just "user"
                    }
                ]
            , version = 1
            , deleted = False
            }

    context =
        makeContext "foo" "user"
            & withAttribute "email" "test@example.com"
            & withAttribute "name" "bob"

testCanDetectRecursiveSegments :: Test
testCanDetectRecursiveSegments = TestCase $ do
    client@(Client clientI) <- makeTestClient
    insertFlag (getField @"store" clientI) flag >>= (pure () @=?)
    insertSegment (getField @"store" clientI) segmentA >>= (pure () @=?)
    insertSegment (getField @"store" clientI) segmentB >>= (pure () @=?)
    boolVariationDetail client "a" (makeContext "b" "user") False >>= (expected @=?)
  where
    flag =
        (makeTestFlag "a" 1)
            { on = True
            , rules =
                [ Rule
                    { clauses =
                        [ Clause
                            { attribute = makeLiteral "key"
                            , contextKind = "user"
                            , op = OpSegmentMatch
                            , values = [String "segmentA"]
                            , negate = False
                            }
                        ]
                    , variationOrRollout =
                        VariationOrRollout
                            { variation = Just 0
                            , rollout = Nothing
                            }
                    , id = "rule-1"
                    , trackEvents = False
                    }
                ]
            , offVariation = Nothing
            }
    segmentA =
        Segment
            { key = "segmentA"
            , version = 1
            , deleted = False
            , included = mempty
            , includedContexts = mempty
            , excluded = mempty
            , excludedContexts = mempty
            , salt = ""
            , rules =
                [ SegmentRule
                    { clauses =
                        [ Clause
                            { attribute = makeLiteral "rule-1"
                            , contextKind = "user"
                            , op = OpSegmentMatch
                            , values = [String "segmentB"]
                            , negate = False
                            }
                        ]
                    , id = "rule-1"
                    , weight = Nothing
                    , bucketBy = Nothing
                    , rolloutContextKind = Just "user"
                    }
                ]
            }
    segmentB =
        Segment
            { key = "segmentB"
            , version = 1
            , deleted = False
            , included = mempty
            , includedContexts = mempty
            , excluded = mempty
            , excludedContexts = mempty
            , salt = ""
            , rules =
                [ SegmentRule
                    { clauses =
                        [ Clause
                            { attribute = makeLiteral "rule-1"
                            , contextKind = "user"
                            , op = OpSegmentMatch
                            , values = [String "segmentA"]
                            , negate = False
                            }
                        ]
                    , id = "rule-1"
                    , weight = Nothing
                    , bucketBy = Nothing
                    , rolloutContextKind = Just "user"
                    }
                ]
            }
    expected =
        EvaluationDetail
            { value = False
            , variationIndex = Nothing
            , reason = EvaluationReasonError {errorKind = EvalErrorKindMalformedFlag}
            }

allTests :: Test
allTests =
    TestList
        [ testExplicitIncludeUser
        , testExplicitIncludeContextKind
        , testExplicitExcludeUser
        , testExplicitExcludeContextKind
        , testExplicitIncludeHasPrecedence
        , testExplicitIncludeContextsHasPrecedence
        , testNeitherIncludedNorExcluded
        , testMatchingRuleWithFullRollout
        , testMatchingRuleWithZeroRollout
        , testMatchingRuleWithMultipleClauses
        , testNonMatchingRuleWithMultipleClauses
        , testCanDetectRecursiveSegments
        ]
