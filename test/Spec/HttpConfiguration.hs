module Spec.HttpConfiguration (allTests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (nub)
import Data.Maybe (isJust)
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import Network.HTTP.Types (HeaderName)
import Test.HUnit

import LaunchDarkly.Server.Config (makeConfig)
import LaunchDarkly.Server.Config.HttpConfiguration
    ( HttpConfiguration (..)
    , instanceIdHeader
    , makeInstanceIdHeader
    )
import LaunchDarkly.Server.Config.HttpConfigurationInternal (makeHttpConfiguration)

parseUUID :: BS.ByteString -> Maybe UUID.UUID
parseUUID = UUID.fromText . TE.decodeUtf8

testInstanceIdHeaderConstantName :: Test
testInstanceIdHeaderConstantName = TestCase $ do
    -- 'HeaderName' is a case-insensitive 'ByteString'; comparison against the literal here
    -- (lifted via OverloadedStrings) verifies we are using the spelling required by the spec.
    assertEqual "" ("X-LaunchDarkly-Instance-Id" :: HeaderName) instanceIdHeader

testMakeInstanceIdHeaderProducesV4 :: Test
testMakeInstanceIdHeaderProducesV4 = TestCase $ do
    (name, value) <- makeInstanceIdHeader
    assertEqual "header name is X-LaunchDarkly-Instance-Id" instanceIdHeader name
    case parseUUID value of
        Nothing -> assertFailure $ "value is not a parseable UUID: " <> BSC.unpack value
        Just uuid ->
            -- A version-4 UUID has version nibble 0x4 in byte index 6 (high nibble).
            -- Data.UUID exposes this via 'UUID.toWords' (w2 high byte is version).
            let (_, w2, _, _) = UUID.toWords uuid
                versionNibble = (w2 `div` 0x1000) `mod` 0x10
             in assertEqual ("instance id " <> show uuid <> " must be UUID v4") 4 versionNibble

testMakeInstanceIdHeaderProducesUniqueIds :: Test
testMakeInstanceIdHeaderProducesUniqueIds = TestCase $ do
    -- Sample a handful of calls; any duplicate within this small set would indicate the GUID
    -- source is broken (the v4 namespace has ~5.3e36 values, so genuine collisions are impossible).
    ids <- mapM (const (snd <$> makeInstanceIdHeader)) [1 .. (10 :: Int)]
    assertEqual "successive instance ids must all differ" (length ids) (length (nub ids))

testMakeHttpConfigurationAttachesInstanceIdHeader :: Test
testMakeHttpConfigurationAttachesInstanceIdHeader = TestCase $ do
    httpConfig <- makeHttpConfiguration "test-version" (makeConfig "sdk-key")
    let headers = defaultRequestHeaders httpConfig
    case lookup instanceIdHeader headers of
        Nothing -> assertFailure "X-LaunchDarkly-Instance-Id header missing from defaultRequestHeaders"
        Just value -> case parseUUID value of
            Nothing -> assertFailure $ "instance id is not a parseable UUID: " <> BSC.unpack value
            Just _ -> pure ()

testMakeHttpConfigurationSetsExactlyOneInstanceIdHeader :: Test
testMakeHttpConfigurationSetsExactlyOneInstanceIdHeader = TestCase $ do
    httpConfig <- makeHttpConfiguration "test-version" (makeConfig "sdk-key")
    let headers = defaultRequestHeaders httpConfig
        matches = filter ((== instanceIdHeader) . fst) headers
    assertEqual "exactly one X-LaunchDarkly-Instance-Id header should be set" 1 (length matches)

testMakeHttpConfigurationGeneratesDistinctInstanceIds :: Test
testMakeHttpConfigurationGeneratesDistinctInstanceIds = TestCase $ do
    -- Each call to makeHttpConfiguration represents a new SDK instance; each must get its own GUID.
    h1 <- makeHttpConfiguration "test-version" (makeConfig "sdk-key")
    h2 <- makeHttpConfiguration "test-version" (makeConfig "sdk-key")
    let id1 = lookup instanceIdHeader (defaultRequestHeaders h1)
        id2 = lookup instanceIdHeader (defaultRequestHeaders h2)
    assertBool "first instance id present" (isJust id1)
    assertBool "second instance id present" (isJust id2)
    assertBool "each SDK instance should generate its own instance id" (id1 /= id2)

allTests :: Test
allTests =
    TestList
        [ testInstanceIdHeaderConstantName
        , testMakeInstanceIdHeaderProducesV4
        , testMakeInstanceIdHeaderProducesUniqueIds
        , testMakeHttpConfigurationAttachesInstanceIdHeader
        , testMakeHttpConfigurationSetsExactlyOneInstanceIdHeader
        , testMakeHttpConfigurationGeneratesDistinctInstanceIds
        ]
