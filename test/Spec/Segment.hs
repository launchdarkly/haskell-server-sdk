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
testExplicitIncludeUser = True ~=? (segmentContainsContext segment user) where
    segment = Segment
        { key      = "test"
        , included = HS.fromList ["foo"]
        , excluded = HS.empty
        , salt     = "abcdef"
        , rules    = []
        , version  = 1
        , deleted  = False
        }

    user = makeContext "foo" "user"

testExplicitExcludeUser :: Test
testExplicitExcludeUser = False ~=? (segmentContainsContext segment user) where
    segment = Segment
        { key      = "test"
        , included = HS.empty
        , excluded = HS.fromList ["foo"]
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
        , excluded = HS.fromList ["foo"]
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
        , excluded = HS.fromList [""]
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
        , excluded = HS.empty
        , salt     = "abcdef"
        , rules    =
            [ SegmentRule
                { id       = "rule"
                , clauses  =
                    [ Clause
                        { attribute = "email"
                        , negate    = False
                        , op        = OpIn
                        , values    = [String "test@example.com"]
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
        , excluded = HS.empty
        , salt     = "abcdef"
        , rules    =
            [ SegmentRule
                { id       = "rule"
                , clauses  =
                    [ Clause
                        { attribute = "email"
                        , negate    = False
                        , op        = OpIn
                        , values    = [String "test@example.com"]
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
        , excluded = HS.empty
        , salt     = "abcdef"
        , rules    =
            [ SegmentRule
                { id       = "rule"
                , clauses  =
                    [ Clause
                        { attribute = "email"
                        , negate    = False
                        , op        = OpIn
                        , values    = [String "test@example.com"]
                        }
                    , Clause
                        { attribute = "name"
                        , negate    = False
                        , op        = OpIn
                        , values    = ["bob"]
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
        , excluded = HS.empty
        , salt     = "abcdef"
        , rules    =
            [ SegmentRule
                { id       = "rule"
                , clauses  =
                    [ Clause
                        { attribute = "email"
                        , negate    = False
                        , op        = OpIn
                        , values    = [String "test@example.com"]
                        }
                    , Clause
                        { attribute = "name"
                        , negate    = False
                        , op        = OpIn
                        , values    = ["bill"]
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
    , testExplicitExcludeUser
    , testExplicitIncludeHasPrecedence
    , testNeitherIncludedNorExcluded
    , testMatchingRuleWithFullRollout
    , testMatchingRuleWithZeroRollout
    , testMatchingRuleWithMultipleClauses
    , testNonMatchingRuleWithMultipleClauses
    ]
