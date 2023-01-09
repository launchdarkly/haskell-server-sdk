module Spec.Segment (allTests) where

import           Test.HUnit
import           Data.Aeson.Types        (Value(..))
import           Data.Function           ((&))
import qualified Data.HashSet as         HS

import LaunchDarkly.Server.Client
import LaunchDarkly.Server.Features
import LaunchDarkly.Server.User
import LaunchDarkly.Server.User.Internal
import LaunchDarkly.Server.Operators
import LaunchDarkly.Server.Evaluate
import LaunchDarkly.Server.Context (makeContext, withAttribute)

testExplicitIncludeUser :: Test
testExplicitIncludeUser = TestCase $ do
    assertEqual "" True (segmentContainsContext segment user)
    assertEqual "" False (segmentContainsContext segment org)

    where

    segment = Segment
        { key      = "test"
        , included = HS.fromList ["foo"]
        , includedContexts = mempty
        , excluded = HS.empty
        , excludedContexts = mempty
        , salt     = "abcdef"
        , rules    = []
        , version  = 1
        , deleted  = False
        }

    user = makeContext "foo" "user"
    org = makeContext "foo" "org"

testExplicitIncludeContextKind :: Test
testExplicitIncludeContextKind = TestCase $ do
    assertEqual "" True $ (segmentContainsContext segment user)
    assertEqual "" False $ (segmentContainsContext segment org)

    where

    segment = Segment
        { key      = "test"
        , included = HS.empty
        , includedContexts = [SegmentTarget { values = HS.fromList ["foo"] , contextKind = "user" }]
        , excluded = HS.empty
        , excludedContexts = mempty
        , salt     = "abcdef"
        , rules    = []
        , version  = 1
        , deleted  = False
        }

    user = makeContext "foo" "user"
    org = makeContext "foo" "org"

testExplicitExcludeUser :: Test
testExplicitExcludeUser = False ~=? (segmentContainsContext segment user) where
    segment = Segment
        { key      = "test"
        , included = HS.empty
        , includedContexts = mempty
        , excluded = HS.fromList ["foo"]
        , excludedContexts = mempty
        , salt     = "abcdef"
        , rules    = []
        , version  = 1
        , deleted  = False
        }

    user = makeContext "foo" "user"

testExplicitExcludeContextKind :: Test
testExplicitExcludeContextKind = False ~=? (segmentContainsContext segment user) where
    segment = Segment
        { key      = "test"
        , included = HS.empty
        , includedContexts = mempty
        , excluded = HS.empty
        , excludedContexts = [SegmentTarget { values = HS.fromList ["foo"] , contextKind = "user" }]
        , salt     = "abcdef"
        , rules    = []
        , version  = 1
        , deleted  = False
        }

    user = makeContext "foo" "user"

testExplicitIncludeHasPrecedence :: Test
testExplicitIncludeHasPrecedence = True ~=? (segmentContainsContext segment user) where
    segment = Segment
        { key      = "test"
        , included = HS.fromList ["foo"]
        , includedContexts = mempty
        , excluded = HS.fromList ["foo"]
        , excludedContexts = mempty
        , salt     = "abcdef"
        , rules    = []
        , version  = 1
        , deleted  = False
        }

    user = makeContext "foo" "user"

testExplicitIncludeContextsHasPrecedence :: Test
testExplicitIncludeContextsHasPrecedence = True ~=? (segmentContainsContext segment user) where
    segment = Segment
        { key      = "test"
        , included = HS.empty
        , includedContexts = [SegmentTarget { values = HS.fromList ["foo"] , contextKind = "user" }]
        , excluded = HS.fromList ["foo"]
        , excludedContexts = [SegmentTarget { values = HS.fromList ["foo"] , contextKind = "user" }]
        , salt     = "abcdef"
        , rules    = []
        , version  = 1
        , deleted  = False
        }

    user = makeContext "foo" "user"

testNeitherIncludedNorExcluded :: Test
testNeitherIncludedNorExcluded = False ~=? (segmentContainsContext segment user) where
    segment = Segment
        { key      = "test"
        , included = HS.fromList [""]
        , includedContexts = mempty
        , excluded = HS.fromList [""]
        , excludedContexts = mempty
        , salt     = "abcdef"
        , rules    = []
        , version  = 1
        , deleted  = False
        }

    user = makeContext "foo" "user"

testMatchingRuleWithFullRollout :: Test
testMatchingRuleWithFullRollout = True ~=? (segmentContainsContext segment context) where
    segment = Segment
        { key      = "test"
        , included = HS.empty
        , includedContexts = mempty
        , excluded = HS.empty
        , excludedContexts = mempty
        , salt     = "abcdef"
        , rules    =
            [ SegmentRule
                { id       = "rule"
                , clauses  =
                    [ Clause
                        { attribute   = "email"
                        , contextKind = "user"
                        , negate      = False
                        , op          = OpIn
                        , values      = [String "test@example.com"]
                        }
                    ]
                , weight   = Just 100000
                , bucketBy = Nothing
                }
            ]
        , version  = 1
        , deleted  = False
        }

    context = makeContext "foo" "user" & withAttribute "email" "test@example.com"

testMatchingRuleWithZeroRollout :: Test
testMatchingRuleWithZeroRollout = False ~=? (segmentContainsContext segment context) where
    segment = Segment
        { key      = "test"
        , included = HS.empty
        , includedContexts = mempty
        , excluded = HS.empty
        , excludedContexts = mempty
        , salt     = "abcdef"
        , rules    =
            [ SegmentRule
                { id       = "rule"
                , clauses  =
                    [ Clause
                        { attribute   = "email"
                        , contextKind = "user"
                        , negate      = False
                        , op          = OpIn
                        , values      = [String "test@example.com"]
                        }
                    ]
                , weight   = Just 0
                , bucketBy = Nothing
                }
            ]
        , version  = 1
        , deleted  = False
        }

    context = makeContext "foo" "user" & withAttribute "email" "test@example.com"

testMatchingRuleWithMultipleClauses :: Test
testMatchingRuleWithMultipleClauses = True ~=? (segmentContainsContext segment context) where
    segment = Segment
        { key      = "test"
        , included = HS.empty
        , includedContexts = mempty
        , excluded = HS.empty
        , excludedContexts = mempty
        , salt     = "abcdef"
        , rules    =
            [ SegmentRule
                { id       = "rule"
                , clauses  =
                    [ Clause
                        { attribute   = "email"
                        , contextKind = "user"
                        , negate      = False
                        , op          = OpIn
                        , values      = [String "test@example.com"]
                        }
                    , Clause
                        { attribute   = "name"
                        , contextKind = "user"
                        , negate      = False
                        , op          = OpIn
                        , values      = ["bob"]
                        }
                    ]
                , weight   = Nothing
                , bucketBy = Nothing
                }
            ]
        , version  = 1
        , deleted  = False
        }

    context = makeContext "foo" "user"
        & withAttribute "email" "test@example.com"
        & withAttribute "name" "bob"

testNonMatchingRuleWithMultipleClauses :: Test
testNonMatchingRuleWithMultipleClauses = False ~=? (segmentContainsContext segment context) where
    segment = Segment
        { key      = "test"
        , included = HS.empty
        , includedContexts = mempty
        , excluded = HS.empty
        , excludedContexts = mempty
        , salt     = "abcdef"
        , rules    =
            [ SegmentRule
                { id       = "rule"
                , clauses  =
                    [ Clause
                        { attribute   = "email"
                        , contextKind = "user"
                        , negate      = False
                        , op          = OpIn
                        , values      = [String "test@example.com"]
                        }
                    , Clause
                        { attribute   = "name"
                        , contextKind = "user"
                        , negate      = False
                        , op          = OpIn
                        , values      = ["bill"]
                        }
                    ]
                , weight   = Nothing
                , bucketBy = Nothing
                }
            ]
        , version  = 1
        , deleted  = False
        }

    context = makeContext "foo" "user"
        & withAttribute "email" "test@example.com"
        & withAttribute "name" "bob"

allTests :: Test
allTests = TestList
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
    ]
