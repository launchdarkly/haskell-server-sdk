module Spec.Bucket (allTests) where

import           Test.HUnit
import           Data.Aeson.Types                  (Value(..))
import qualified Data.HashMap.Strict as            HM
import           Data.HashMap.Strict               (HashMap)
import           Data.Function                     ((&))

import           LaunchDarkly.Server.User
import           LaunchDarkly.Server.User.Internal
import           LaunchDarkly.Server.Client
import           LaunchDarkly.Server.Features
import           LaunchDarkly.Server.Evaluate

testBucketUserByKey :: Test
testBucketUserByKey = TestList
    [ TestCase $ assertEqual "bucket one" 0.42157587 (bucketUser (unwrapUser $ makeUser "userKeyA") "hashKey" "key" "saltyA" Nothing)
    , TestCase $ assertEqual "bucket two" 0.6708485 (bucketUser (unwrapUser $ makeUser "userKeyB") "hashKey" "key" "saltyA" Nothing)
    , TestCase $ assertEqual "bucket three" 0.10343106 (bucketUser (unwrapUser $ makeUser "userKeyC") "hashKey" "key" "saltyA" Nothing)
    ]

testBucketUserWithSeed :: Test
testBucketUserWithSeed = TestList
    [ TestCase $ assertEqual "bucket one" 0.09801207 (bucketUser (unwrapUser $ makeUser "userKeyA") "hashKey" "key" "saltyA" (Just 61))
    , TestCase $ assertEqual "bucket two" 0.14483777 (bucketUser (unwrapUser $ makeUser "userKeyB") "hashKey" "key" "saltyA" (Just 61))
    , TestCase $ assertEqual "bucket three" 0.9242641 (bucketUser (unwrapUser $ makeUser "userKeyC") "hashKey" "key" "saltyA" (Just 61))
    ]

testBucketUserByIntAttr :: Test
testBucketUserByIntAttr = TestList
    [ TestCase $ assertEqual "intAttr" 0.54771423 $ bucketUser (unwrapUser $ (makeUser "userKeyD")
        & userSetCustom (HM.singleton "intAttr" (Number 33333))) "hashKey" "intAttr" "saltyA" Nothing
    , TestCase $ assertEqual "stringAttr" 0.54771423 $ bucketUser (unwrapUser $ (makeUser "userKeyD")
        & userSetCustom (HM.singleton "stringAttr" (String "33333"))) "hashKey" "stringAttr" "saltyA" Nothing
    ]

testBucketUserByFloatAttrNotAllowed :: Test
testBucketUserByFloatAttrNotAllowed = (~=?) 0 $ bucketUser (unwrapUser $ (makeUser "userKeyE")
    & userSetCustom (HM.singleton "floatAttr" (Number 999.999))) "hashKey" "floatAttr" "saltyA" Nothing

testBucketUserByFloatAttrThatIsReallyAnIntIsAllowed :: Test
testBucketUserByFloatAttrThatIsReallyAnIntIsAllowed = (~=?) 0.54771423 $ bucketUser (unwrapUser $ (makeUser "userKeyE")
    & userSetCustom (HM.singleton "floatAttr" (Number 33333))) "hashKey" "floatAttr" "saltyA" Nothing

testVariationIndexForUser :: Test
testVariationIndexForUser = TestCase $ do
    assertEqual "test" (Just 0, False) $ variationIndexForUser rollout (unwrapUser $ makeUser "userKeyA") "hashKey" "saltyA"
    assertEqual "test" (Just 1, True)  $ variationIndexForUser rollout (unwrapUser $ makeUser "userKeyB") "hashKey" "saltyA"
    assertEqual "test" (Just 0, False) $ variationIndexForUser rollout (unwrapUser $ makeUser "userKeyC") "hashKey" "saltyA"

    where

    rollout = VariationOrRollout
        { variation = Nothing
        , rollout   = Just Rollout
            { bucketBy   = Nothing
            , seed       = Nothing
            , kind       = RolloutKindExperiment
            , variations =
                [ WeightedVariation
                    { variation = 0
                    , weight    = 60000
                    , untracked = True
                    }
                , WeightedVariation
                    { variation = 1
                    , weight    = 40000
                    , untracked = False
                    }
                ]
            }
        }

allTests :: Test
allTests = TestList
    [ testBucketUserByKey
    , testBucketUserWithSeed
    , testBucketUserByIntAttr
    , testBucketUserByFloatAttrNotAllowed
    , testBucketUserByFloatAttrThatIsReallyAnIntIsAllowed
    , testVariationIndexForUser
    ]
