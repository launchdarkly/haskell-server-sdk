module Spec.Features (allTests) where

import Data.ByteString.Lazy
import Data.Aeson
import Data.Text as T
import LaunchDarkly.Server.Features
import Test.HUnit

assertFlagClientAvailabilityValues :: ByteString -> Bool -> Bool -> Bool -> IO ()
assertFlagClientAvailabilityValues jsonString expectedExplicit expectedUsingMobileKey expectedUsingEnvironmentId =
    case (decode jsonString) of
      Just Flag { clientSideAvailability = ClientSideAvailability { usingMobileKey = usingMobileKey, usingEnvironmentId = usingEnvironmentId, explicit = explicit }, .. } -> do
          assertEqual "explicit did not match expected value" expectedExplicit explicit
          assertEqual "usingEnvironmentId did not match expected value" expectedUsingEnvironmentId usingEnvironmentId
          assertEqual "usingMobileKey did not match expected value" expectedUsingMobileKey usingMobileKey
      Nothing -> assertFailure "Failed to decode into flag"

testCanDecodeOldSchema :: Test
testCanDecodeOldSchema = TestCase $ do
    let json = "{\
        \\"trackEventsFallthrough\":false,\
        \\"rules\":[],\
        \\"offVariation\":null,\
        \\"fallthrough\":{\"rollout\":null,\
        \\"variation\":null},\
        \\"key\":\"flag-key\",\
        \\"version\":1,\
        \\"variations\":[],\
        \\"salt\":\"\",\
        \\"targets\":[],\
        \\"prerequisites\":[],\
        \\"deleted\":false,\
        \\"trackEvents\":false,\
        \\"debugEventsUntilDate\":null,\
        \\"on\":true,\
        \\"clientSide\": true\
    \}"
    assertFlagClientAvailabilityValues json False True True
    case decode json :: Maybe Flag of
      Just flag -> do
          assertBool "Re-encoding retains clientSide" ("clientSide\": true" `T.isInfixOf` json)
          assertBool "Re-encoding does not include new clientSideAvailability" (not $ "clientSideAvailability" `T.isInfixOf` json)
      Nothing -> assertFailure "Failed to decode into flag"

testCanDecodeNewSchema :: Test
testCanDecodeNewSchema = TestCase $ do
    let json = "{\
        \\"trackEventsFallthrough\":false,\
        \\"rules\":[],\
        \\"offVariation\":null,\
        \\"fallthrough\":{\"rollout\":null,\
        \\"variation\":null},\
        \\"key\":\"flag-key\",\
        \\"version\":1,\
        \\"variations\":[],\
        \\"salt\":\"\",\
        \\"targets\":[],\
        \\"prerequisites\":[],\
        \\"deleted\":false,\
        \\"trackEvents\":false,\
        \\"debugEventsUntilDate\":null,\
        \\"on\":true,\
        \\"clientSideAvailability\": {\
            \\"usingMobileKey\": false,\
            \\"usingEnvironmentId\": false\
        \}\
    \}"
    assertFlagClientAvailabilityValues json True False False
    case decode json :: Maybe Flag of
      Just flag -> do
          assertBool "Re-encoding does not include clientSide" (not $ ("clientSide\": true" `T.isInfixOf` json))
          assertBool "Re-encoding does include new clientSideAvailability" ("clientSideAvailability" `T.isInfixOf` json)
          assertBool "Re-encoding sets usingMobileKey correctly" ("usingMobileKey\": false" `T.isInfixOf` json)
          assertBool "Re-encoding sets usingEnvironmentId correctly" ("usingEnvironmentId\": false" `T.isInfixOf` json)
      Nothing -> assertFailure "Failed to decode into flag"

allTests :: Test
allTests = TestList
    [ testCanDecodeNewSchema
    , testCanDecodeOldSchema
    ]
