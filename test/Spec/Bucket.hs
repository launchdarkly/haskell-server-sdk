module Spec.Bucket (allTests) where

import Test.HUnit
import Data.Aeson.Types (Value(..))
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Function ((&))

import LaunchDarkly.Server.User
import LaunchDarkly.Server.Client
import LaunchDarkly.Server.Features
import LaunchDarkly.Server.Evaluate

testBucketUserByKey :: Test
testBucketUserByKey = TestList
    [ TestCase $ assertEqual "bucket one" 0.42157587 (bucketUser (makeUser "userKeyA") "hashKey" "key" "saltyA")
    , TestCase $ assertEqual "bucket two" 0.6708485 (bucketUser (makeUser "userKeyB") "hashKey" "key" "saltyA")
    , TestCase $ assertEqual "bucket three" 0.10343106 (bucketUser (makeUser "userKeyC") "hashKey" "key" "saltyA")
    ]

testBucketUserByIntAttr :: Test
testBucketUserByIntAttr = TestList
    [ TestCase $ assertEqual "intAttr" 0.54771423 $ bucketUser ((makeUser "userKeyD")
        & userSetCustom (HM.singleton "intAttr" (Number 33333))) "hashKey" "intAttr" "saltyA"
    , TestCase $ assertEqual "stringAttr" 0.54771423 $ bucketUser ((makeUser "userKeyD")
        & userSetCustom (HM.singleton "stringAttr" (String "33333"))) "hashKey" "stringAttr" "saltyA"
    ]

testBucketUserByFloatAttrNotAllowed :: Test
testBucketUserByFloatAttrNotAllowed = (~=?) 0 $ bucketUser ((makeUser "userKeyE")
    & userSetCustom (HM.singleton "floatAttr" (Number 999.999))) "hashKey" "floatAttr" "saltyA"

testBucketUserByFloatAttrThatIsReallyAnIntIsAllowed :: Test
testBucketUserByFloatAttrThatIsReallyAnIntIsAllowed = (~=?) 0.54771423 $ bucketUser ((makeUser "userKeyE")
    & userSetCustom (HM.singleton "floatAttr" (Number 33333))) "hashKey" "floatAttr" "saltyA"

testVariationIndexForUser :: Test
testVariationIndexForUser = TestCase $ do
    assertEqual "test" (Just 0) $ variationIndexForUser rollout (makeUser "userKeyA") "hashKey" "saltyA"
    assertEqual "test" (Just 1) $ variationIndexForUser rollout (makeUser "userKeyB") "hashKey" "saltyA"
    assertEqual "test" (Just 0) $ variationIndexForUser rollout (makeUser "userKeyC") "hashKey" "saltyA"

    where

    rollout = VariationOrRollout
        { variation = Nothing
        , rollout   = Just Rollout
            { bucketBy   = Nothing
            , variations =
                [ WeightedVariation
                    { variation = 0
                    , weight    = 60000
                    }
                , WeightedVariation
                    { variation = 1
                    , weight    = 40000
                    }
                ]
            }
        }

allTests :: Test
allTests = TestList
    [ testBucketUserByKey
    , testBucketUserByIntAttr
    , testBucketUserByFloatAttrNotAllowed
    , testBucketUserByFloatAttrThatIsReallyAnIntIsAllowed
    , testVariationIndexForUser
    ]
