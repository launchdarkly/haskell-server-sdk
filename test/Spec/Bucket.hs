module Spec.Bucket (allTests) where

import           Test.HUnit
import           Data.Aeson.Types                  (Value(..))
import qualified Data.HashMap.Strict as            HM
import           Data.HashMap.Strict               (HashMap)
import           Data.Function                     ((&))

import           LaunchDarkly.AesonCompat (singleton)
import           LaunchDarkly.Server.User
import           LaunchDarkly.Server.User.Internal
import           LaunchDarkly.Server.Client
import           LaunchDarkly.Server.Features
import           LaunchDarkly.Server.Evaluate
import LaunchDarkly.Server.Context (makeContext, withAttribute)

testBucketUserByKey :: Test
testBucketUserByKey = TestList
    [ TestCase $ assertEqual "bucket one" 0.42157587 (bucketContext (makeContext "userKeyA" "user") "hashKey" "key" "saltyA" Nothing)
    , TestCase $ assertEqual "bucket two" 0.6708485 (bucketContext (makeContext "userKeyB" "user") "hashKey" "key" "saltyA" Nothing)
    , TestCase $ assertEqual "bucket three" 0.10343106 (bucketContext (makeContext "userKeyC" "user") "hashKey" "key" "saltyA" Nothing)
    ]

testBucketUserWithSeed :: Test
testBucketUserWithSeed = TestList
    [ TestCase $ assertEqual "bucket one" 0.09801207 (bucketContext (makeContext "userKeyA" "user") "hashKey" "key" "saltyA" (Just 61))
    , TestCase $ assertEqual "bucket two" 0.14483777 (bucketContext (makeContext "userKeyB" "user") "hashKey" "key" "saltyA" (Just 61))
    , TestCase $ assertEqual "bucket three" 0.9242641 (bucketContext (makeContext "userKeyC" "user") "hashKey" "key" "saltyA" (Just 61))
    ]

testBucketUserByIntAttr :: Test
testBucketUserByIntAttr = TestList
    [ TestCase $ assertEqual "intAttr" 0.54771423 $ bucketContext (makeContext "userKeyD" "user" & withAttribute "intAttr" (Number 33333)) "hashKey" "intAttr" "saltyA" Nothing
    , TestCase $ assertEqual "stringAttr" 0.54771423 $ bucketContext (makeContext "userKeyD" "user" & withAttribute "stringAttr" (String "33333")) "hashKey" "stringAttr" "saltyA" Nothing
    ]

testBucketUserByFloatAttrNotAllowed :: Test
testBucketUserByFloatAttrNotAllowed = (~=?) 0 $ bucketContext (makeContext "userKeyE" "user" & withAttribute "floatAttr" (Number 999.999)) "hashKey" "floatAttr" "saltyA" Nothing

testBucketUserByFloatAttrThatIsReallyAnIntIsAllowed :: Test
testBucketUserByFloatAttrThatIsReallyAnIntIsAllowed = (~=?) 0.54771423 $ bucketContext (makeContext "userKeyE" "user" & withAttribute "floatAttr" (Number 33333)) "hashKey" "floatAttr" "saltyA" Nothing

testVariationIndexForUser :: Test
testVariationIndexForUser = TestCase $ do
    assertEqual "test" (Just 0, False) $ variationIndexForContext rollout (makeContext "userKeyA" "user") "hashKey" "saltyA"
    assertEqual "test" (Just 1, True)  $ variationIndexForContext rollout (makeContext "userKeyB" "user") "hashKey" "saltyA"
    assertEqual "test" (Just 0, False) $ variationIndexForContext rollout (makeContext "userKeyC" "user") "hashKey" "saltyA"

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
