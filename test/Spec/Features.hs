module Spec.Features (allTests) where

import Data.Aeson
import Data.ByteString.Lazy
import Data.Generics.Product (getField)
import Data.Text as T
import LaunchDarkly.Server.Features
import Test.HUnit

assertFlagClientAvailabilityValues :: ByteString -> Bool -> Bool -> Bool -> IO ()
assertFlagClientAvailabilityValues jsonString expectedExplicit expectedUsingMobileKey expectedUsingEnvironmentId =
    case (decode jsonString) of
        Just Flag {clientSideAvailability = ClientSideAvailability {usingMobileKey = usingMobileKey, usingEnvironmentId = usingEnvironmentId, explicit = explicit}, ..} -> do
            assertEqual "explicit did not match expected value" expectedExplicit explicit
            assertEqual "usingEnvironmentId did not match expected value" expectedUsingEnvironmentId usingEnvironmentId
            assertEqual "usingMobileKey did not match expected value" expectedUsingMobileKey usingMobileKey
        Nothing -> assertFailure "Failed to decode into flag"

testCanDecodeOldSchema :: Test
testCanDecodeOldSchema = TestCase $ do
    let json =
            "{\
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
    let json =
            "{\
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

decodeOrFail :: FromJSON a => ByteString -> IO a
decodeOrFail json = case eitherDecode json of
    Right value -> pure value
    Left err -> assertFailure $ "Failed to decode: " ++ err

testOmittedOrNullFlagPropertiesTakeTheirDefaults :: Test
testOmittedOrNullFlagPropertiesTakeTheirDefaults = TestCase $ do
    withDefaults <-
        decodeOrFail
            "{\"key\":\"flag-key\",\"version\":1,\"on\":true,\"fallthrough\":{\"variation\":0},\"variations\":[true,false],\
            \\"trackEvents\":false,\"trackEventsFallthrough\":false,\"deleted\":false,\"clientSide\":false,\
            \\"prerequisites\":[],\"salt\":\"\",\"targets\":[],\"contextTargets\":[],\"rules\":[]}"
    omitted <-
        decodeOrFail
            "{\"key\":\"flag-key\",\"version\":1,\"on\":true,\"fallthrough\":{\"variation\":0},\"variations\":[true,false]}"
    nulls <-
        decodeOrFail
            "{\"key\":\"flag-key\",\"version\":1,\"on\":true,\"fallthrough\":{\"variation\":0,\"rollout\":null},\"variations\":[true,false],\
            \\"trackEvents\":null,\"trackEventsFallthrough\":null,\"deleted\":null,\"clientSide\":null,\"clientSideAvailability\":null,\
            \\"prerequisites\":null,\"salt\":null,\"targets\":null,\"contextTargets\":null,\"rules\":null,\
            \\"offVariation\":null,\"debugEventsUntilDate\":null}"
    -- A flag without its optional properties, or with them set to null, equals a flag with the default values.
    assertEqual "omitted properties" (withDefaults :: Flag) omitted
    assertEqual "null properties" withDefaults nulls

testOmittedOrNullRuleAndClausePropertiesTakeTheirDefaults :: Test
testOmittedOrNullRuleAndClausePropertiesTakeTheirDefaults = TestCase $ do
    let flagWithRule rule =
            "{\"key\":\"flag-key\",\"version\":1,\"on\":true,\"fallthrough\":{\"variation\":0},\"variations\":[true,false],\"rules\":["
                <> rule
                <> "]}"
    withDefaults <-
        decodeOrFail $
            flagWithRule
                "{\"id\":\"rule-id\",\"variation\":1,\"trackEvents\":false,\
                \\"clauses\":[{\"attribute\":\"key\",\"op\":\"in\",\"values\":[\"a\"],\"negate\":false}]}"
    omitted <-
        decodeOrFail $
            flagWithRule "{\"id\":\"rule-id\",\"variation\":1,\"clauses\":[{\"attribute\":\"key\",\"op\":\"in\",\"values\":[\"a\"]}]}"
    nulls <-
        decodeOrFail $
            flagWithRule
                "{\"id\":\"rule-id\",\"variation\":1,\"rollout\":null,\"trackEvents\":null,\
                \\"clauses\":[{\"attribute\":\"key\",\"op\":\"in\",\"values\":[\"a\"],\"negate\":null,\"contextKind\":null}]}"
    -- A rule or clause without its optional properties, or with them set to null, equals one with the default values.
    assertEqual "omitted properties" (withDefaults :: Flag) omitted
    assertEqual "null properties" withDefaults nulls

testOmittedOrNullSegmentPropertiesTakeTheirDefaults :: Test
testOmittedOrNullSegmentPropertiesTakeTheirDefaults = TestCase $ do
    withDefaults <-
        decodeOrFail
            "{\"key\":\"segment-key\",\"version\":1,\"included\":[],\"includedContexts\":[],\"excluded\":[],\"excludedContexts\":[],\
            \\"salt\":\"\",\"deleted\":false,\
            \\"rules\":[{\"id\":\"\",\"clauses\":[{\"attribute\":\"key\",\"op\":\"in\",\"values\":[\"a\"]}]}]}"
    omitted <-
        decodeOrFail
            "{\"key\":\"segment-key\",\"version\":1,\"rules\":[{\"clauses\":[{\"attribute\":\"key\",\"op\":\"in\",\"values\":[\"a\"]}]}]}"
    nulls <-
        decodeOrFail
            "{\"key\":\"segment-key\",\"version\":1,\"included\":null,\"includedContexts\":null,\"excluded\":null,\"excludedContexts\":null,\
            \\"salt\":null,\"deleted\":null,\
            \\"rules\":[{\"id\":null,\"weight\":null,\"bucketBy\":null,\"rolloutContextKind\":null,\
            \\"clauses\":[{\"attribute\":\"key\",\"op\":\"in\",\"values\":[\"a\"]}]}]}"
    onlyRequired <- decodeOrFail "{\"key\":\"segment-key\",\"version\":1}"
    -- A segment without its optional properties, or with them set to null, equals one with the default values.
    assertEqual "omitted properties" (withDefaults :: Segment) omitted
    assertEqual "null properties" withDefaults nulls
    assertEqual "only required properties" [] (getField @"rules" (onlyRequired :: Segment))

allTests :: Test
allTests =
    TestList
        [ testCanDecodeNewSchema
        , testCanDecodeOldSchema
        , testOmittedOrNullFlagPropertiesTakeTheirDefaults
        , testOmittedOrNullRuleAndClausePropertiesTakeTheirDefaults
        , testOmittedOrNullSegmentPropertiesTakeTheirDefaults
        ]
