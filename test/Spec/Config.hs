module Spec.Config (allTests) where
import Test.HUnit
import LaunchDarkly.Server (makeApplicationInfo)
import LaunchDarkly.Server.Config.Internal (makeApplicationInfo, withApplicationValue, getApplicationInfoHeader)
import Control.Lens ((&))

testEmptyApplicationInfoGeneratesNoHeader :: Test
testEmptyApplicationInfoGeneratesNoHeader = TestCase $ do
    assertEqual "" Nothing $ getApplicationInfoHeader $ makeApplicationInfo

testEmptyApplicationInfoIgnoresInvalidKeys :: Test
testEmptyApplicationInfoIgnoresInvalidKeys = TestCase $ do
    assertEqual "" emptyInfo modified

    where

    emptyInfo = makeApplicationInfo
    modified = withApplicationValue "invalid-key" "value" emptyInfo
        & withApplicationValue "another-invalid-key" "value"

testEmptyApplicationInfoIgnoresInvalidValues :: Test
testEmptyApplicationInfoIgnoresInvalidValues = TestCase $ do
    assertEqual "" emptyInfo modified

    where

    emptyInfo = makeApplicationInfo
    modified = withApplicationValue "id" " " emptyInfo
        & withApplicationValue "id" "&"
        & withApplicationValue "id" "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._-a"

testCorrectlyGeneratesHeaderText :: Test
testCorrectlyGeneratesHeaderText = TestCase $ do
    assertEqual "" (Just "application-id/my-id application-version/my-version") value
    assertEqual "" (Just "application-id/my-id application-version/my-version") ignoreError

    where

    info = makeApplicationInfo
        & withApplicationValue "id" "my-id"
        & withApplicationValue "version" "my-version"
    value = getApplicationInfoHeader info
    ignoreError = withApplicationValue "version" "should-ignore-@me" info & getApplicationInfoHeader

allTests :: Test
allTests = TestList
    [ testEmptyApplicationInfoGeneratesNoHeader
    , testEmptyApplicationInfoIgnoresInvalidKeys
    , testEmptyApplicationInfoIgnoresInvalidValues
    , testCorrectlyGeneratesHeaderText
    ]
