module Spec.Evaluate (allTests) where

import           Test.HUnit
import           Data.Aeson
import           Data.Aeson.Types                  (Value(..))
import qualified Data.HashMap.Strict as            HM
import           Data.HashMap.Strict               (HashMap)
import           Data.Function                     ((&))
import           Data.Generics.Product             (getField)

import           LaunchDarkly.Server.Store
import           LaunchDarkly.Server.Store.Internal
import           LaunchDarkly.Server.Client
import           LaunchDarkly.Server.Client.Internal
import           LaunchDarkly.Server.User
import           LaunchDarkly.Server.User.Internal
import           LaunchDarkly.Server.Features
import           LaunchDarkly.Server.Operators
import           LaunchDarkly.Server.Details
import           LaunchDarkly.Server.Store.Internal
import           LaunchDarkly.Server.Evaluate
import           LaunchDarkly.Server.Config

import           Util.Features

makeEmptyStore :: IO (StoreHandle IO)
makeEmptyStore = do
    handle <- makeStoreIO Nothing 0
    initializeStore handle mempty mempty
    pure handle

testFlagReturnsOffVariationIfFlagIsOff :: Test
testFlagReturnsOffVariationIfFlagIsOff = TestCase $ do
    store <- makeEmptyStore
    x <- evaluateDetail flag user store
    assertEqual "test" expected x

    where

    expected = (EvaluationDetail
        { value          = String "off"
        , variationIndex = pure 1
        , reason         = EvaluationReasonOff
        }, [])

    user = unwrapUser $ makeUser "x"

    flag = Flag
        { key                    = "feature"
        , version                = 1
        , on                     = False
        , trackEvents            = False
        , trackEventsFallthrough = False
        , deleted                = False
        , prerequisites          = []
        , salt                   = ""
        , targets                = []
        , rules                  = []
        , fallthrough            = VariationOrRollout
            { variation = Just 0
            , rollout   = Nothing
            }
        , offVariation           = Just 1
        , variations             = [String "fall", String "off", String "on"]
        , debugEventsUntilDate   = Nothing
        }

testFlagReturnsFallthroughIfFlagIsOnAndThereAreNoRules :: Test
testFlagReturnsFallthroughIfFlagIsOnAndThereAreNoRules = TestCase $ do
    store <- makeEmptyStore
    x <- evaluateDetail flag user store
    assertEqual "test" expected x

    where

    expected = (EvaluationDetail
        { value          = String "fall"
        , variationIndex = pure 0
        , reason         = EvaluationReasonFallthrough
        }, [])

    user = unwrapUser $ makeUser "x"

    flag = Flag
        { key                    = "feature"
        , version                = 1
        , on                     = True
        , trackEvents            = False
        , trackEventsFallthrough = False
        , deleted                = False
        , prerequisites          = []
        , salt                   = ""
        , targets                = []
        , rules                  = []
        , fallthrough            = VariationOrRollout
            { variation = Just 0
            , rollout   = Nothing
            }
        , offVariation           = Just 1
        , variations             = [String "fall", String "off", String "on"]
        , debugEventsUntilDate   = Nothing
        }

testFlagReturnsErrorIfFallthroughHasTooHighVariation :: Test
testFlagReturnsErrorIfFallthroughHasTooHighVariation = TestCase $ do
    client@(Client clientI) <- makeTestClient
    insertFlag (getField @"store" clientI) flag >>= (pure () @=?)
    stringVariationDetail client "a" (makeUser "b") "default" >>= (expected @=?)
    where
        flag = (makeTestFlag "a" 52)
            { on           = True
            , offVariation = Nothing
            , fallthrough  = VariationOrRollout
                { variation = Just 999
                , rollout   = Nothing
                }
            , variations   =
                [ String "abc"
                , String "123"
                , String "456"
                ]
            }
        expected = EvaluationDetail
            { value          = "default"
            , variationIndex = Nothing
            , reason         = EvaluationReasonError EvalErrorKindMalformedFlag
            }

testFlagReturnsErrorIfFallthroughHasNeitherVariationNorRollout :: Test
testFlagReturnsErrorIfFallthroughHasNeitherVariationNorRollout = TestCase $ do
    client@(Client clientI) <- makeTestClient
    insertFlag (getField @"store" clientI) flag >>= (pure () @=?)
    stringVariationDetail client "a" (makeUser "b") "default" >>= (expected @=?)
    where
        flag = (makeTestFlag "a" 52)
            { on           = True
            , offVariation = Nothing
            , fallthrough  = VariationOrRollout
                { variation = Nothing
                , rollout   = Nothing
                }
            , variations   = [String "abc"]
            }
        expected = EvaluationDetail
            { value          = "default"
            , variationIndex = Nothing
            , reason         = EvaluationReasonError EvalErrorKindMalformedFlag
            }

testFlagReturnsErrorIfFallthroughHasEmptyRolloutVariationList :: Test
testFlagReturnsErrorIfFallthroughHasEmptyRolloutVariationList = TestCase $ do
    client@(Client clientI) <- makeTestClient
    insertFlag (getField @"store" clientI) flag >>= (pure () @=?)
    stringVariationDetail client "a" (makeUser "b") "default" >>= (expected @=?)
    where
        flag = (makeTestFlag "a" 52)
            { on           = True
            , offVariation = Nothing
            , fallthrough  = VariationOrRollout
                { variation = Nothing
                , rollout   = pure Rollout
                    { variations = []
                    , bucketBy   = pure "key"
                    }
                }
            , variations   = [String "abc"]
            }
        expected = EvaluationDetail
            { value          = "default"
            , variationIndex = Nothing
            , reason         = EvaluationReasonError EvalErrorKindMalformedFlag
            }

testFlagReturnsOffVariationIfPrerequisiteIsNotFound :: Test
testFlagReturnsOffVariationIfPrerequisiteIsNotFound = TestCase $ do
    client@(Client clientI) <- makeTestClient
    insertFlag (getField @"store" clientI) flag >>= (pure () @=?)
    stringVariationDetail client "a" (makeUser "b") "default" >>= (expected @=?)
    where
        flag = (makeTestFlag "a" 52)
            { on            = True
            , offVariation  = pure 1
            , fallthrough   = VariationOrRollout
                { variation = pure 0
                , rollout   = Nothing
                }
            , variations    = [String "fall", String "off", String "on"]
            , prerequisites =
                [ Prerequisite
                    { key       = "feature1"
                    , variation = 1
                    }
                ]
            }
        expected = EvaluationDetail
            { value          = "off"
            , variationIndex = pure 1
            , reason         = EvaluationReasonPrerequisiteFailed "feature1"
            }

testFlagReturnsOffVariationIfPrerequisiteIsOff :: Test
testFlagReturnsOffVariationIfPrerequisiteIsOff = TestCase $ do
    client@(Client clientI) <- makeTestClient
    insertFlag (getField @"store" clientI) flag0                     >>= (pure () @=?)
    insertFlag (getField @"store" clientI) flag1                     >>= (pure () @=?)
    stringVariationDetail client "feature0" (makeUser "b") "default" >>= (expected @=?)
    where
        flag0 = (makeTestFlag "feature0" 52)
            { on            = True
            , offVariation  = pure 1
            , fallthrough   = VariationOrRollout
                { variation = pure 0
                , rollout   = Nothing
                }
            , variations    = [String "fall", String "off", String "on"]
            , prerequisites =
                [ Prerequisite
                    { key       = "feature1"
                    , variation = 1
                    }
                ]
            }
        flag1 = (makeTestFlag "feature1" 52)
            { on            = False
            , offVariation  = pure 1
            , fallthrough   = VariationOrRollout
                { variation = pure 0
                , rollout   = Nothing
                }
            , variations    = [String "nogo", String "go"]
            }
        expected = EvaluationDetail
            { value          = "off"
            , variationIndex = pure 1
            , reason         = EvaluationReasonPrerequisiteFailed "feature1"
            }

testFlagReturnsOffVariationIfPrerequisiteIsNotMet :: Test
testFlagReturnsOffVariationIfPrerequisiteIsNotMet = TestCase $ do
    client@(Client clientI) <- makeTestClient
    insertFlag (getField @"store" clientI) flag0                     >>= (pure () @=?)
    insertFlag (getField @"store" clientI) flag1                     >>= (pure () @=?)
    stringVariationDetail client "feature0" (makeUser "b") "default" >>= (expected @=?)
    where
        flag0 = (makeTestFlag "feature0" 52)
            { on            = True
            , offVariation  = pure 1
            , fallthrough   = VariationOrRollout
                { variation = pure 0
                , rollout   = Nothing
                }
            , variations    = [String "fall", String "off", String "on"]
            , prerequisites =
                [ Prerequisite
                    { key       = "feature1"
                    , variation = 1
                    }
                ]
            }
        flag1 = (makeTestFlag "feature1" 52)
            { on            = True
            , offVariation  = pure 1
            , fallthrough   = VariationOrRollout
                { variation = pure 0
                , rollout   = Nothing
                }
            , variations    = [String "nogo", String "go"]
            }
        expected = EvaluationDetail
            { value          = "off"
            , variationIndex = pure 1
            , reason         = EvaluationReasonPrerequisiteFailed "feature1"
            }

testFlagReturnsFallthroughVariationIfPrerequisiteIsMetAndThereAreNoRules :: Test
testFlagReturnsFallthroughVariationIfPrerequisiteIsMetAndThereAreNoRules = TestCase $ do
    client@(Client clientI) <- makeTestClient
    insertFlag (getField @"store" clientI) flag0                     >>= (pure () @=?)
    insertFlag (getField @"store" clientI) flag1                     >>= (pure () @=?)
    stringVariationDetail client "feature0" (makeUser "b") "default" >>= (expected @=?)
    where
        flag0 = (makeTestFlag "feature0" 52)
            { on            = True
            , offVariation  = pure 1
            , fallthrough   = VariationOrRollout
                { variation = pure 0
                , rollout   = Nothing
                }
            , variations    = [String "fall", String "off", String "on"]
            , prerequisites =
                [ Prerequisite
                    { key       = "feature1"
                    , variation = 1
                    }
                ]
            }
        flag1 = (makeTestFlag "feature1" 52)
            { on            = True
            , offVariation  = pure 1
            , fallthrough   = VariationOrRollout
                { variation = pure 1
                , rollout   = Nothing
                }
            , variations    = [String "nogo", String "go"]
            }
        expected = EvaluationDetail
            { value          = "fall"
            , variationIndex = pure 0
            , reason         = EvaluationReasonFallthrough
            }

testClauseCanMatchCustomAttribute :: Test
testClauseCanMatchCustomAttribute = TestCase $ do
    store <- makeStoreIO Nothing 0
    x <- evaluateDetail flag user store
    assertEqual "test" expected x

    where

    expected = (EvaluationDetail
        { value          = Bool True
        , variationIndex = pure 1
        , reason         = EvaluationReasonRuleMatch
            { ruleIndex = 0
            , ruleId    = "clause"
            }
        }, [])

    user = unwrapUser $ (makeUser "x")
        & userSetName   (pure "bob")
        & userSetCustom (HM.fromList [("legs", Number 4)])

    flag = Flag
        { key                    = "feature"
        , version                = 1
        , on                     = True
        , trackEvents            = False
        , trackEventsFallthrough = False
        , deleted                = False
        , prerequisites          = []
        , salt                   = ""
        , targets                = []
        , rules                  =
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
                , trackEvents        = False
                }
            ]
        , fallthrough            = VariationOrRollout
            { variation = Just 0
            , rollout   = Nothing
            }
        , offVariation           = Just 0
        , variations             = [Bool False, Bool True]
        , debugEventsUntilDate   = Nothing
        }

makeTestClient :: IO Client
makeTestClient = do
    (Client client) <- makeClient $ (makeConfig "") & configSetOffline True
    initializeStore (getField @"store" client) mempty mempty
    pure (Client client)

testEvaluatingUnknownFlagReturnsDefault :: Test
testEvaluatingUnknownFlagReturnsDefault = TestCase $ do
    client <- makeTestClient
    boolVariation client "a" (makeUser "b") False >>= (False @=?)

testEvaluatingUnknownFlagReturnsDefaultWithDetail :: Test
testEvaluatingUnknownFlagReturnsDefaultWithDetail = TestCase $ do
    client <- makeTestClient
    boolVariationDetail client "a" (makeUser "b") False >>= (expected @=?)
    where
        expected = EvaluationDetail
            { value          = False
            , variationIndex = Nothing
            , reason         = EvaluationReasonError EvalErrorFlagNotFound
            }

testDefaultIsReturnedIfFlagEvaluatesToNil :: Test
testDefaultIsReturnedIfFlagEvaluatesToNil = TestCase $ do
    client@(Client clientI) <- makeTestClient
    insertFlag (getField @"store" clientI) flag >>= (pure () @=?)
    boolVariation client "a" (makeUser "b") False >>= (False @=?)
    where
        flag = (makeTestFlag "a" 52)
            { on           = False
            , offVariation = Nothing
            }

testDefaultIsReturnedIfFlagEvaluatesToNilWithDetail :: Test
testDefaultIsReturnedIfFlagEvaluatesToNilWithDetail = TestCase $ do
    client@(Client clientI) <- makeTestClient
    insertFlag (getField @"store" clientI) flag >>= (pure () @=?)
    boolVariationDetail client "a" (makeUser "b") False >>= (expected @=?)
    where
        flag = (makeTestFlag "a" 52)
            { on           = False
            , offVariation = Nothing
            }
        expected = EvaluationDetail
            { value          = False
            , variationIndex = Nothing
            , reason         = EvaluationReasonOff
            }

allTests :: Test
allTests = TestList
    [ testFlagReturnsOffVariationIfFlagIsOff
    , testFlagReturnsFallthroughIfFlagIsOnAndThereAreNoRules
    , testFlagReturnsErrorIfFallthroughHasTooHighVariation
    , testFlagReturnsErrorIfFallthroughHasNeitherVariationNorRollout
    , testFlagReturnsErrorIfFallthroughHasEmptyRolloutVariationList
    , testFlagReturnsOffVariationIfPrerequisiteIsNotFound
    , testFlagReturnsOffVariationIfPrerequisiteIsOff
    , testFlagReturnsOffVariationIfPrerequisiteIsNotMet
    , testFlagReturnsFallthroughVariationIfPrerequisiteIsMetAndThereAreNoRules
    , testClauseCanMatchCustomAttribute
    , testEvaluatingUnknownFlagReturnsDefault
    , testEvaluatingUnknownFlagReturnsDefaultWithDetail
    , testDefaultIsReturnedIfFlagEvaluatesToNil
    , testDefaultIsReturnedIfFlagEvaluatesToNilWithDetail
    ]
