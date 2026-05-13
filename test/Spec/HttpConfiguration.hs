module Spec.HttpConfiguration (allTests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (isJust)
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import Network.HTTP.Types (HeaderName)
import Test.HUnit

import LaunchDarkly.Server.Client.Internal (makeHttpConfiguration)
import LaunchDarkly.Server.Config (makeConfig)
import LaunchDarkly.Server.Config.HttpConfiguration (HttpConfiguration (..))

instanceIdHeaderName :: HeaderName
instanceIdHeaderName = "X-LaunchDarkly-Instance-Id"

parseUUID :: BS.ByteString -> Maybe UUID.UUID
parseUUID = UUID.fromText . TE.decodeUtf8

testMakeHttpConfigurationAttachesInstanceIdHeader :: Test
testMakeHttpConfigurationAttachesInstanceIdHeader = TestCase $ do
    httpConfig <- makeHttpConfiguration (makeConfig "sdk-key")
    let headers = defaultRequestHeaders httpConfig
    case lookup instanceIdHeaderName headers of
        Nothing -> assertFailure "X-LaunchDarkly-Instance-Id header missing from defaultRequestHeaders"
        Just value -> case parseUUID value of
            Nothing -> assertFailure $ "instance id is not a parseable UUID: " <> BSC.unpack value
            Just uuid ->
                let (_, w2, _, _) = UUID.toWords uuid
                    versionNibble = (w2 `div` 0x1000) `mod` 0x10
                 in assertEqual ("instance id " <> show uuid <> " must be UUID v4") 4 versionNibble

testMakeHttpConfigurationSetsExactlyOneInstanceIdHeader :: Test
testMakeHttpConfigurationSetsExactlyOneInstanceIdHeader = TestCase $ do
    httpConfig <- makeHttpConfiguration (makeConfig "sdk-key")
    let headers = defaultRequestHeaders httpConfig
        matches = filter ((== instanceIdHeaderName) . fst) headers
    assertEqual "exactly one X-LaunchDarkly-Instance-Id header should be set" 1 (length matches)

testMakeHttpConfigurationGeneratesDistinctInstanceIds :: Test
testMakeHttpConfigurationGeneratesDistinctInstanceIds = TestCase $ do
    h1 <- makeHttpConfiguration (makeConfig "sdk-key")
    h2 <- makeHttpConfiguration (makeConfig "sdk-key")
    let id1 = lookup instanceIdHeaderName (defaultRequestHeaders h1)
        id2 = lookup instanceIdHeaderName (defaultRequestHeaders h2)
    assertBool "first instance id present" (isJust id1)
    assertBool "second instance id present" (isJust id2)
    assertBool "each SDK instance should generate its own instance id" (id1 /= id2)

allTests :: Test
allTests =
    TestList
        [ testMakeHttpConfigurationAttachesInstanceIdHeader
        , testMakeHttpConfigurationSetsExactlyOneInstanceIdHeader
        , testMakeHttpConfigurationGeneratesDistinctInstanceIds
        ]
