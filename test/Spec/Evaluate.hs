module Spec.Evaluate (allTests) where

import Test.HUnit
import Data.Aeson
import Data.Aeson.Types (Value(..))
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Function ((&))

import LaunchDarkly.Server.Store
import LaunchDarkly.Server.Client
import LaunchDarkly.Server.User
import LaunchDarkly.Server.Features
import LaunchDarkly.Server.Operators
import LaunchDarkly.Server.Details
import LaunchDarkly.Server.Store.Memory
import LaunchDarkly.Server.Evaluate

testFlagReturnsOffVariationIfFlagIsOff :: Test
testFlagReturnsOffVariationIfFlagIsOff = TestCase $ do
    store <- makeMemoryStoreIO
    x <- evaluateDetail flag user store
    assertEqual "test" expected x

    where

    expected = (EvaluationDetail
        { value          = String "off"
        , variationIndex = pure 1
        , reason         = EvaluationReasonOff
        }, [])

    user = makeUser "x"

    flag = Flag
        { key                  = "feature"
        , version              = 1
        , on                   = False
        , trackEvents          = False
        , deleted              = False
        , prerequisites        = []
        , salt                 = ""
        , sel                  = ""
        , targets              = []
        , rules                = []
        , fallthrough          = VariationOrRollout
            { variation = Just 0
            , rollout   = Nothing
            }
        , offVariation         = Just 1
        , variations           = [String "fall", String "off", String "on"]
        , debugEventsUntilDate = Nothing
        }

testFlagReturnsFallthroughIfFlagIsOnAndThereAreNoRules :: Test
testFlagReturnsFallthroughIfFlagIsOnAndThereAreNoRules = TestCase $ do
    store <- makeMemoryStoreIO
    x <- evaluateDetail flag user store
    assertEqual "test" expected x

    where

    expected = (EvaluationDetail
        { value          = String "fall"
        , variationIndex = pure 0
        , reason         = EvaluationReasonFallthrough
        }, [])

    user = makeUser "x"

    flag = Flag
        { key                  = "feature"
        , version              = 1
        , on                   = True
        , trackEvents          = False
        , deleted              = False
        , prerequisites        = []
        , salt                 = ""
        , sel                  = ""
        , targets              = []
        , rules                = []
        , fallthrough          = VariationOrRollout
            { variation = Just 0
            , rollout   = Nothing
            }
        , offVariation         = Just 1
        , variations           = [String "fall", String "off", String "on"]
        , debugEventsUntilDate = Nothing
        }

testClauseCanMatchCustomAttribute :: Test
testClauseCanMatchCustomAttribute = TestCase $ do
    store <- makeMemoryStoreIO
    x <- evaluateDetail flag user store
    assertEqual "test" expected x

    where

    expected = (EvaluationDetail
        { value          = Bool True
        , variationIndex = pure 0
        , reason         = EvaluationReasonFallthrough
        }, [])

    user = (makeUser "x")
        { name   = pure "bob"
        , custom = HM.fromList [("legs", Number 4)]
        }

    flag = Flag
        { key                  = "feature"
        , version              = 1
        , on                   = True
        , trackEvents          = False
        , deleted              = False
        , prerequisites        = []
        , salt                 = ""
        , sel                  = ""
        , targets              = []
        , rules                =
            [ Rule
                { clauses            =
                    [ Clause
                        { attribute = "name"
                        , op        = OpIn
                        , values    = [String "bob"]
                        , negate    = False
                        }
                    ]
                , variationOrRollout = VariationOrRollout
                    { variation = Just 1
                    , rollout   = Nothing
                    }
                , id                 = "clause"
                }
            ]
        , fallthrough          = VariationOrRollout
            { variation = Just 0
            , rollout   = Nothing
            }
        , offVariation         = Just 0
        , variations           = [Bool False, Bool True]
        , debugEventsUntilDate = Nothing
        }

allTests :: Test
allTests = TestList
    [ testFlagReturnsOffVariationIfFlagIsOff
    , testFlagReturnsFallthroughIfFlagIsOnAndThereAreNoRules
    -- , testClauseCanMatchCustomAttribute
    ]
