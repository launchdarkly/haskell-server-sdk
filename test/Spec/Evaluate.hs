module Spec.Evaluate (allTests) where

import Data.Aeson (Value (..))
import Data.Function ((&))
import Data.Generics.Product (getField)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Test.HUnit

import LaunchDarkly.Server.Client
import LaunchDarkly.Server.Client.Internal
import LaunchDarkly.Server.Config
import LaunchDarkly.Server.Context
import LaunchDarkly.Server.Details
import LaunchDarkly.Server.Evaluate
import LaunchDarkly.Server.Features
import LaunchDarkly.Server.Operators
import LaunchDarkly.Server.Store
import LaunchDarkly.Server.Store.Internal
import LaunchDarkly.Server.User
import LaunchDarkly.Server.User.Internal

import LaunchDarkly.AesonCompat (fromList)
import LaunchDarkly.Server.Reference (makeLiteral, makeReference)
import Util.Features

makeEmptyStore :: IO (StoreHandle IO)
makeEmptyStore = do
    handle <- makeStoreIO Nothing 0
    initializeStore handle mempty mempty
    pure handle

testFlagReturnsOffVariationIfFlagIsOff :: Test
testFlagReturnsOffVariationIfFlagIsOff = TestCase $ do
    store <- makeEmptyStore
    x <- evaluateDetail flag context HS.empty store
    assertEqual "test" expected x
  where
    expected =
        ( EvaluationDetail
            { value = String "off"
            , variationIndex = pure 1
            , reason = EvaluationReasonOff
            }
        , []
        )

    context = makeContext "x" "user"

    flag =
        Flag
            { key = "feature"
            , version = 1
            , on = False
            , trackEvents = False
            , trackEventsFallthrough = False
            , deleted = False
            , prerequisites = []
            , salt = ""
            , targets = []
            , contextTargets = []
            , rules = []
            , fallthrough =
                VariationOrRollout
                    { variation = Just 0
                    , rollout = Nothing
                    }
            , offVariation = Just 1
            , variations = [String "fall", String "off", String "on"]
            , debugEventsUntilDate = Nothing
            , clientSideAvailability = ClientSideAvailability {usingEnvironmentId = True, usingMobileKey = False, explicit = True}
            }

testFlagReturnsFallthroughIfFlagIsOnAndThereAreNoRules :: Test
testFlagReturnsFallthroughIfFlagIsOnAndThereAreNoRules = TestCase $ do
    store <- makeEmptyStore
    x <- evaluateDetail flag context HS.empty store
    assertEqual "test" expected x
  where
    expected =
        ( EvaluationDetail
            { value = String "fall"
            , variationIndex = pure 0
            , reason =
                EvaluationReasonFallthrough
                    { inExperiment = False
                    }
            }
        , []
        )

    context = makeContext "x" "user"

    flag =
        Flag
            { key = "feature"
            , version = 1
            , on = True
            , trackEvents = False
            , trackEventsFallthrough = False
            , deleted = False
            , prerequisites = []
            , salt = ""
            , targets = []
            , contextTargets = []
            , rules = []
            , fallthrough =
                VariationOrRollout
                    { variation = Just 0
                    , rollout = Nothing
                    }
            , offVariation = Just 1
            , variations = [String "fall", String "off", String "on"]
            , debugEventsUntilDate = Nothing
            , clientSideAvailability = ClientSideAvailability {usingEnvironmentId = True, usingMobileKey = False, explicit = True}
            }

testFlagReturnsErrorIfFallthroughHasTooHighVariation :: Test
testFlagReturnsErrorIfFallthroughHasTooHighVariation = TestCase $ do
    client@(Client clientI) <- makeTestClient
    insertFlag (getField @"store" clientI) flag >>= (pure () @=?)
    stringVariationDetail client "a" (makeContext "b" "user") "default" >>= (expected @=?)
  where
    flag =
        (makeTestFlag "a" 52)
            { on = True
            , offVariation = Nothing
            , fallthrough =
                VariationOrRollout
                    { variation = Just 999
                    , rollout = Nothing
                    }
            , variations =
                [ String "abc"
                , String "123"
                , String "456"
                ]
            }
    expected =
        EvaluationDetail
            { value = "default"
            , variationIndex = Nothing
            , reason = EvaluationReasonError EvalErrorKindMalformedFlag
            }

testFlagReturnsErrorIfFallthroughHasNeitherVariationNorRollout :: Test
testFlagReturnsErrorIfFallthroughHasNeitherVariationNorRollout = TestCase $ do
    client@(Client clientI) <- makeTestClient
    insertFlag (getField @"store" clientI) flag >>= (pure () @=?)
    stringVariationDetail client "a" (makeContext "b" "user") "default" >>= (expected @=?)
  where
    flag =
        (makeTestFlag "a" 52)
            { on = True
            , offVariation = Nothing
            , fallthrough =
                VariationOrRollout
                    { variation = Nothing
                    , rollout = Nothing
                    }
            , variations = [String "abc"]
            }
    expected =
        EvaluationDetail
            { value = "default"
            , variationIndex = Nothing
            , reason = EvaluationReasonError EvalErrorKindMalformedFlag
            }

testFlagReturnsErrorIfFallthroughHasEmptyRolloutVariationList :: Test
testFlagReturnsErrorIfFallthroughHasEmptyRolloutVariationList = TestCase $ do
    client@(Client clientI) <- makeTestClient
    insertFlag (getField @"store" clientI) flag >>= (pure () @=?)
    stringVariationDetail client "a" (makeContext "b" "user") "default" >>= (expected @=?)
  where
    flag =
        (makeTestFlag "a" 52)
            { on = True
            , offVariation = Nothing
            , fallthrough =
                VariationOrRollout
                    { variation = Nothing
                    , rollout =
                        pure
                            Rollout
                                { variations = []
                                , seed = Nothing
                                , kind = RolloutKindRollout
                                , bucketBy = pure "key"
                                , contextKind = Just "user"
                                }
                    }
            , variations = [String "abc"]
            }
    expected =
        EvaluationDetail
            { value = "default"
            , variationIndex = Nothing
            , reason = EvaluationReasonError EvalErrorKindMalformedFlag
            }

testFlagReturnsErrorIfThereIsAPrerequisiteCycle :: Test
testFlagReturnsErrorIfThereIsAPrerequisiteCycle = TestCase $ do
    client@(Client clientI) <- makeTestClient
    insertFlag (getField @"store" clientI) flag0 >>= (pure () @=?)
    insertFlag (getField @"store" clientI) flag1 >>= (pure () @=?)
    stringVariationDetail client "feature0" (makeContext "b" "user") "default" >>= (expected @=?)
  where
    flag0 =
        (makeTestFlag "feature0" 52)
            { on = True
            , offVariation = pure 1
            , fallthrough =
                VariationOrRollout
                    { variation = pure 0
                    , rollout = Nothing
                    }
            , variations = [String "fall", String "off", String "on"]
            , prerequisites =
                [ Prerequisite
                    { key = "feature1"
                    , variation = 1
                    }
                ]
            }
    flag1 =
        (makeTestFlag "feature1" 52)
            { on = True
            , offVariation = pure 1
            , fallthrough =
                VariationOrRollout
                    { variation = pure 0
                    , rollout = Nothing
                    }
            , variations = [String "nogo", String "go"]
            , prerequisites =
                [ Prerequisite
                    { key = "feature0"
                    , variation = 1
                    }
                ]
            }
    expected =
        EvaluationDetail
            { value = "default"
            , variationIndex = Nothing
            , reason = EvaluationReasonError EvalErrorKindMalformedFlag
            }

testFlagReturnsOffVariationIfPrerequisiteIsNotFound :: Test
testFlagReturnsOffVariationIfPrerequisiteIsNotFound = TestCase $ do
    client@(Client clientI) <- makeTestClient
    insertFlag (getField @"store" clientI) flag >>= (pure () @=?)
    stringVariationDetail client "a" (makeContext "b" "user") "default" >>= (expected @=?)
  where
    flag =
        (makeTestFlag "a" 52)
            { on = True
            , offVariation = pure 1
            , fallthrough =
                VariationOrRollout
                    { variation = pure 0
                    , rollout = Nothing
                    }
            , variations = [String "fall", String "off", String "on"]
            , prerequisites =
                [ Prerequisite
                    { key = "feature1"
                    , variation = 1
                    }
                ]
            }
    expected =
        EvaluationDetail
            { value = "off"
            , variationIndex = pure 1
            , reason = EvaluationReasonPrerequisiteFailed "feature1"
            }

testFlagReturnsOffVariationIfPrerequisiteIsOff :: Test
testFlagReturnsOffVariationIfPrerequisiteIsOff = TestCase $ do
    client@(Client clientI) <- makeTestClient
    insertFlag (getField @"store" clientI) flag0 >>= (pure () @=?)
    insertFlag (getField @"store" clientI) flag1 >>= (pure () @=?)
    stringVariationDetail client "feature0" (makeContext "b" "user") "default" >>= (expected @=?)
  where
    flag0 =
        (makeTestFlag "feature0" 52)
            { on = True
            , offVariation = pure 1
            , fallthrough =
                VariationOrRollout
                    { variation = pure 0
                    , rollout = Nothing
                    }
            , variations = [String "fall", String "off", String "on"]
            , prerequisites =
                [ Prerequisite
                    { key = "feature1"
                    , variation = 1
                    }
                ]
            }
    flag1 =
        (makeTestFlag "feature1" 52)
            { on = False
            , offVariation = pure 1
            , fallthrough =
                VariationOrRollout
                    { variation = pure 0
                    , rollout = Nothing
                    }
            , variations = [String "nogo", String "go"]
            }
    expected =
        EvaluationDetail
            { value = "off"
            , variationIndex = pure 1
            , reason = EvaluationReasonPrerequisiteFailed "feature1"
            }

testFlagReturnsOffVariationIfPrerequisiteIsNotMet :: Test
testFlagReturnsOffVariationIfPrerequisiteIsNotMet = TestCase $ do
    client@(Client clientI) <- makeTestClient
    insertFlag (getField @"store" clientI) flag0 >>= (pure () @=?)
    insertFlag (getField @"store" clientI) flag1 >>= (pure () @=?)
    stringVariationDetail client "feature0" (makeContext "b" "user") "default" >>= (expected @=?)
  where
    flag0 =
        (makeTestFlag "feature0" 52)
            { on = True
            , offVariation = pure 1
            , fallthrough =
                VariationOrRollout
                    { variation = pure 0
                    , rollout = Nothing
                    }
            , variations = [String "fall", String "off", String "on"]
            , prerequisites =
                [ Prerequisite
                    { key = "feature1"
                    , variation = 1
                    }
                ]
            }
    flag1 =
        (makeTestFlag "feature1" 52)
            { on = True
            , offVariation = pure 1
            , fallthrough =
                VariationOrRollout
                    { variation = pure 0
                    , rollout = Nothing
                    }
            , variations = [String "nogo", String "go"]
            }
    expected =
        EvaluationDetail
            { value = "off"
            , variationIndex = pure 1
            , reason = EvaluationReasonPrerequisiteFailed "feature1"
            }

testFlagReturnsFallthroughVariationIfPrerequisiteIsMetAndThereAreNoRules :: Test
testFlagReturnsFallthroughVariationIfPrerequisiteIsMetAndThereAreNoRules = TestCase $ do
    client@(Client clientI) <- makeTestClient
    insertFlag (getField @"store" clientI) flag0 >>= (pure () @=?)
    insertFlag (getField @"store" clientI) flag1 >>= (pure () @=?)
    stringVariationDetail client "feature0" (makeContext "b" "user") "default" >>= (expected @=?)
  where
    flag0 =
        (makeTestFlag "feature0" 52)
            { on = True
            , offVariation = pure 1
            , fallthrough =
                VariationOrRollout
                    { variation = pure 0
                    , rollout = Nothing
                    }
            , variations = [String "fall", String "off", String "on"]
            , prerequisites =
                [ Prerequisite
                    { key = "feature1"
                    , variation = 1
                    }
                ]
            }
    flag1 =
        (makeTestFlag "feature1" 52)
            { on = True
            , offVariation = pure 1
            , fallthrough =
                VariationOrRollout
                    { variation = pure 1
                    , rollout = Nothing
                    }
            , variations = [String "nogo", String "go"]
            }
    expected =
        EvaluationDetail
            { value = "fall"
            , variationIndex = pure 0
            , reason =
                EvaluationReasonFallthrough
                    { inExperiment = False
                    }
            }

testFlagCanTargetUserKeys :: Test
testFlagCanTargetUserKeys = TestCase $ do
    client@(Client clientI) <- makeTestClient
    insertFlag (getField @"store" clientI) flag >>= (pure () @=?)
    boolVariationDetail client "feature0" (makeContext "user-key" "user") False >>= (expected @=?)
  where
    flag :: Flag =
        (makeTestFlag "feature0" 52)
            { on = True
            , targets = [Target {values = ["user-key"], variation = 1, contextKind = "user"}]
            , variations = [Bool False, Bool True]
            , fallthrough =
                VariationOrRollout
                    { variation = Just 0
                    , rollout = Nothing
                    }
            }
    expected :: EvaluationDetail Bool =
        EvaluationDetail
            { value = True
            , variationIndex = pure 1
            , reason = EvaluationReasonTargetMatch
            }

testFlagCanTargetContextKeys :: Test
testFlagCanTargetContextKeys = TestCase $ do
    client@(Client clientI) <- makeTestClient
    insertFlag (getField @"store" clientI) flag >>= (pure () @=?)
    boolVariationDetail client "feature0" (makeContext "match-key" "user") False >>= (fallthrough @=?)
    boolVariationDetail client "feature0" (makeContext "match-key" "org") False >>= (match @=?)
  where
    flag :: Flag =
        (makeTestFlag "feature0" 52)
            { on = True
            , contextTargets = [Target {values = ["match-key"], variation = 1, contextKind = "org"}]
            , variations = [Bool False, Bool True]
            , fallthrough =
                VariationOrRollout
                    { variation = Just 0
                    , rollout = Nothing
                    }
            }
    match :: EvaluationDetail Bool =
        EvaluationDetail
            { value = True
            , variationIndex = pure 1
            , reason = EvaluationReasonTargetMatch
            }
    fallthrough :: EvaluationDetail Bool =
        EvaluationDetail
            { value = False
            , variationIndex = pure 0
            , reason = EvaluationReasonFallthrough {inExperiment = False}
            }

testFlagCanTargetContextFallsbackToUserTargets :: Test
testFlagCanTargetContextFallsbackToUserTargets = TestCase $ do
    client@(Client clientI) <- makeTestClient
    insertFlag (getField @"store" clientI) flag >>= (pure () @=?)
    boolVariationDetail client "feature0" (makeContext "match-key" "user") False >>= (match @=?)
  where
    flag :: Flag =
        (makeTestFlag "feature0" 52)
            { on = True
            , targets = [Target {values = ["match-key"], variation = 1, contextKind = "user"}]
            , contextTargets = [Target {values = [], variation = 1, contextKind = "user"}]
            , variations = [Bool False, Bool True]
            , fallthrough =
                VariationOrRollout
                    { variation = Just 0
                    , rollout = Nothing
                    }
            }
    match :: EvaluationDetail Bool =
        EvaluationDetail
            { value = True
            , variationIndex = pure 1
            , reason = EvaluationReasonTargetMatch
            }

testFlagChecksTargetsBeforeRules :: Test
testFlagChecksTargetsBeforeRules = TestCase $ do
    client@(Client clientI) <- makeTestClient
    insertFlag (getField @"store" clientI) flag >>= (pure () @=?)
    boolVariationDetail client "feature0" (makeContext "match-key" "user") False >>= (match @=?)
  where
    flag :: Flag =
        (makeTestFlag "feature0" 52)
            { on = True
            , targets = [Target {values = ["match-key"], variation = 0, contextKind = "user"}]
            , rules =
                [ Rule
                    { clauses =
                        [ Clause
                            { attribute = makeLiteral "kind"
                            , contextKind = "user"
                            , op = OpIn
                            , values = [String "user"]
                            , negate = False
                            }
                        ]
                    , variationOrRollout =
                        VariationOrRollout
                            { variation = Just 1
                            , rollout = Nothing
                            }
                    , id = "clause"
                    , trackEvents = False
                    }
                ]
            , variations = [Bool False, Bool True]
            , fallthrough =
                VariationOrRollout
                    { variation = Just 1
                    , rollout = Nothing
                    }
            }
    match :: EvaluationDetail Bool =
        EvaluationDetail
            { value = False
            , variationIndex = pure 0
            , reason = EvaluationReasonTargetMatch
            }

testClauseCanMatchOnKind :: Test
testClauseCanMatchOnKind = TestCase $ do
    store <- makeStoreIO Nothing 0
    orgDetail <- evaluateDetail flag orgContext HS.empty store
    userDetail <- evaluateDetail flag userContext HS.empty store
    multiDetail <- evaluateDetail flag multiContext HS.empty store

    assertEqual "test" expectedMatch orgDetail
    assertEqual "test" expectedFailure userDetail
    assertEqual "test" expectedMatch multiDetail
  where
    expectedMatch =
        ( EvaluationDetail
            { value = Bool True
            , variationIndex = pure 1
            , reason =
                EvaluationReasonRuleMatch
                    { ruleIndex = 0
                    , ruleId = "clause"
                    , inExperiment = False
                    }
            }
        , []
        )

    expectedFailure =
        ( EvaluationDetail
            { value = Bool False
            , variationIndex = pure 0
            , reason = EvaluationReasonFallthrough {inExperiment = False}
            }
        , []
        )

    orgContext = makeContext "x" "org"
    userContext = makeContext "x" "user"
    multiContext = makeMultiContext [orgContext, userContext]

    flag =
        Flag
            { key = "feature"
            , version = 1
            , on = True
            , trackEvents = False
            , trackEventsFallthrough = False
            , deleted = False
            , prerequisites = []
            , salt = ""
            , targets = []
            , contextTargets = []
            , rules =
                [ Rule
                    { clauses =
                        [ Clause
                            { attribute = makeLiteral "kind"
                            , contextKind = "user"
                            , op = OpIn
                            , values = [String "org"]
                            , negate = False
                            }
                        ]
                    , variationOrRollout =
                        VariationOrRollout
                            { variation = Just 1
                            , rollout = Nothing
                            }
                    , id = "clause"
                    , trackEvents = False
                    }
                ]
            , fallthrough =
                VariationOrRollout
                    { variation = Just 0
                    , rollout = Nothing
                    }
            , offVariation = Just 0
            , variations = [Bool False, Bool True]
            , debugEventsUntilDate = Nothing
            , clientSideAvailability = ClientSideAvailability {usingEnvironmentId = True, usingMobileKey = False, explicit = True}
            }

testClauseCanMatchCustomAttribute :: Test
testClauseCanMatchCustomAttribute = TestCase $ do
    store <- makeStoreIO Nothing 0
    userDetail <- evaluateDetail flag userContext HS.empty store
    orgDetail <- evaluateDetail flag orgContext HS.empty store
    assertEqual "test" expectedMatch userDetail
    assertEqual "test" expectedFailure orgDetail
  where
    expectedMatch =
        ( EvaluationDetail
            { value = Bool True
            , variationIndex = pure 1
            , reason =
                EvaluationReasonRuleMatch
                    { ruleIndex = 0
                    , ruleId = "clause"
                    , inExperiment = False
                    }
            }
        , []
        )

    expectedFailure =
        ( EvaluationDetail
            { value = Bool False
            , variationIndex = pure 0
            , reason = EvaluationReasonFallthrough {inExperiment = False}
            }
        , []
        )

    userContext = makeContext "x" "user" & withAttribute "legs" (Number 4)
    orgContext = makeContext "x" "org" & withAttribute "legs" (Number 4)

    flag =
        Flag
            { key = "feature"
            , version = 1
            , on = True
            , trackEvents = False
            , trackEventsFallthrough = False
            , deleted = False
            , prerequisites = []
            , salt = ""
            , targets = []
            , contextTargets = []
            , rules =
                [ Rule
                    { clauses =
                        [ Clause
                            { attribute = makeLiteral "legs"
                            , contextKind = "user"
                            , op = OpIn
                            , values = [Number 4]
                            , negate = False
                            }
                        ]
                    , variationOrRollout =
                        VariationOrRollout
                            { variation = Just 1
                            , rollout = Nothing
                            }
                    , id = "clause"
                    , trackEvents = False
                    }
                ]
            , fallthrough =
                VariationOrRollout
                    { variation = Just 0
                    , rollout = Nothing
                    }
            , offVariation = Just 0
            , variations = [Bool False, Bool True]
            , debugEventsUntilDate = Nothing
            , clientSideAvailability = ClientSideAvailability {usingEnvironmentId = True, usingMobileKey = False, explicit = True}
            }

testClauseCanMatchCustomAttributeReference :: Test
testClauseCanMatchCustomAttributeReference = TestCase $ do
    store <- makeStoreIO Nothing 0
    userDetail <- evaluateDetail flag userContext HS.empty store
    orgDetail <- evaluateDetail flag orgContext HS.empty store
    assertEqual "test" expectedMatch userDetail
  where
    -- assertEqual "test" expectedFailure orgDetail

    expectedMatch =
        ( EvaluationDetail
            { value = Bool True
            , variationIndex = pure 1
            , reason =
                EvaluationReasonRuleMatch
                    { ruleIndex = 0
                    , ruleId = "clause"
                    , inExperiment = False
                    }
            }
        , []
        )

    expectedFailure =
        ( EvaluationDetail
            { value = Bool False
            , variationIndex = pure 0
            , reason = EvaluationReasonFallthrough {inExperiment = False}
            }
        , []
        )

    userContext = makeContext "x" "user" & withAttribute "attr~1a" (String "right")
    orgContext = makeContext "x" "org" & withAttribute "attr~1a" (String "right")

    flag =
        Flag
            { key = "feature"
            , version = 1
            , on = True
            , trackEvents = False
            , trackEventsFallthrough = False
            , deleted = False
            , prerequisites = []
            , salt = ""
            , targets = []
            , contextTargets = []
            , rules =
                [ Rule
                    { clauses =
                        [ Clause
                            { attribute = makeReference "/attr~01a"
                            , contextKind = "user"
                            , op = OpIn
                            , values = [String "right"]
                            , negate = False
                            }
                        ]
                    , variationOrRollout =
                        VariationOrRollout
                            { variation = Just 1
                            , rollout = Nothing
                            }
                    , id = "clause"
                    , trackEvents = False
                    }
                ]
            , fallthrough =
                VariationOrRollout
                    { variation = Just 0
                    , rollout = Nothing
                    }
            , offVariation = Just 0
            , variations = [Bool False, Bool True]
            , debugEventsUntilDate = Nothing
            , clientSideAvailability = ClientSideAvailability {usingEnvironmentId = True, usingMobileKey = False, explicit = True}
            }

makeTestClient :: IO Client
makeTestClient = do
    (Client client) <- makeClient $ (makeConfig "") & configSetOffline True
    initializeStore (getField @"store" client) mempty mempty
    pure (Client client)

testEvaluatingUnknownFlagReturnsDefault :: Test
testEvaluatingUnknownFlagReturnsDefault = TestCase $ do
    client <- makeTestClient
    boolVariation client "a" (makeContext "b" "user") False >>= (False @=?)

testEvaluatingUnknownFlagReturnsDefaultWithDetail :: Test
testEvaluatingUnknownFlagReturnsDefaultWithDetail = TestCase $ do
    client <- makeTestClient
    boolVariationDetail client "a" (makeContext "b" "user") False >>= (expected @=?)
  where
    expected =
        EvaluationDetail
            { value = False
            , variationIndex = Nothing
            , reason = EvaluationReasonError EvalErrorFlagNotFound
            }

testDefaultIsReturnedIfFlagEvaluatesToNil :: Test
testDefaultIsReturnedIfFlagEvaluatesToNil = TestCase $ do
    client@(Client clientI) <- makeTestClient
    insertFlag (getField @"store" clientI) flag >>= (pure () @=?)
    boolVariation client "a" (makeContext "b" "user") False >>= (False @=?)
  where
    flag =
        (makeTestFlag "a" 52)
            { on = False
            , offVariation = Nothing
            }

testDefaultIsReturnedIfFlagEvaluatesToNilWithDetail :: Test
testDefaultIsReturnedIfFlagEvaluatesToNilWithDetail = TestCase $ do
    client@(Client clientI) <- makeTestClient
    insertFlag (getField @"store" clientI) flag >>= (pure () @=?)
    boolVariationDetail client "a" (makeContext "b" "user") False >>= (expected @=?)
  where
    flag =
        (makeTestFlag "a" 52)
            { on = False
            , offVariation = Nothing
            }
    expected =
        EvaluationDetail
            { value = False
            , variationIndex = Nothing
            , reason = EvaluationReasonOff
            }

allTests :: Test
allTests =
    TestList
        [ testFlagReturnsOffVariationIfFlagIsOff
        , testFlagReturnsFallthroughIfFlagIsOnAndThereAreNoRules
        , testFlagReturnsErrorIfFallthroughHasTooHighVariation
        , testFlagReturnsErrorIfFallthroughHasNeitherVariationNorRollout
        , testFlagReturnsErrorIfFallthroughHasEmptyRolloutVariationList
        , testFlagReturnsErrorIfThereIsAPrerequisiteCycle
        , testFlagReturnsOffVariationIfPrerequisiteIsNotFound
        , testFlagReturnsOffVariationIfPrerequisiteIsOff
        , testFlagReturnsOffVariationIfPrerequisiteIsNotMet
        , testFlagReturnsFallthroughVariationIfPrerequisiteIsMetAndThereAreNoRules
        , testFlagCanTargetUserKeys
        , testFlagCanTargetContextKeys
        , testFlagCanTargetContextFallsbackToUserTargets
        , testFlagChecksTargetsBeforeRules
        , testClauseCanMatchCustomAttribute
        , testClauseCanMatchCustomAttributeReference
        , testClauseCanMatchOnKind
        , testEvaluatingUnknownFlagReturnsDefault
        , testEvaluatingUnknownFlagReturnsDefaultWithDetail
        , testDefaultIsReturnedIfFlagEvaluatesToNil
        , testDefaultIsReturnedIfFlagEvaluatesToNilWithDetail
        ]
