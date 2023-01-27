module Spec.Integrations.TestData
    (allTests)
    where

import           Test.HUnit

import           Data.Aeson                                            (ToJSON, toJSON)
import           Data.Function                                         ((&))
import           Data.Functor                                          ((<&>))
import           Data.Text                                             (Text)
import           GHC.Generics                                          (Generic)

import           Control.Monad.Logger
import           LaunchDarkly.Server
import           LaunchDarkly.Server.DataSource.Internal
import qualified LaunchDarkly.Server.Integrations.TestData             as TestData
import qualified LaunchDarkly.Server.Integrations.TestData.FlagBuilder as FlagBuilder

allTests :: Test
allTests = TestList
    [ testVariationForAllUsers
    , testMultipleFlags
    , testModifyFlags
    , testMultipleClients
    , testTargeting
    , testRules
    , testValueForAllUsers
    ]

testConfig :: DataSourceFactory -> Config
testConfig factory =
    configSetSendEvents False $
    configSetDataSourceFactory (Just factory) $
    configSetLogger (runStdoutLoggingT . filterLogger (\_ lvl -> lvl /= LevelDebug)) $
    makeConfig "sdk-key"

testVariationForAllUsers :: Test
testVariationForAllUsers = TestCase $ do
  td <- TestData.newTestData
  TestData.update td =<<
      (TestData.flag td "flag-key-1"
          <&> TestData.variationForAllUsers True)
  let config = testConfig (TestData.dataSourceFactory td)
  client <- makeClient config
  let user1 = makeContext "user1" "user"
      user2 = makeContext "user2" "user"
  assertEqual "user1 set" True =<< boolVariation client "flag-key-1" user1 False
  assertEqual "user2 set" True =<< boolVariation client "flag-key-1" user2 False
  assertEqual "user not set for another flag" False =<< boolVariation client "another-key" user1 False
  close client

testModifyFlags :: Test
testModifyFlags = TestCase $ do
  td <- TestData.newTestData
  TestData.update td =<<
      (TestData.flag td "flag-key-1"
          <&> TestData.variations [ toJSON ("blue" :: Text)
                                  , toJSON ("red" :: Text)
                                  , toJSON ("green" :: Text)
                                  ]
          <&> TestData.variationForAllUsers (0 :: TestData.VariationIndex))
  let config = testConfig (TestData.dataSourceFactory td)
  client <- makeClient config
  let user = makeContext "user" "user"
  assertEqual "user set" "blue" =<< stringVariation client "flag-key-1" user "none"

  TestData.update td =<<
      (TestData.flag td "flag-key-1"
          <&> TestData.variationForAllUsers (2 :: TestData.VariationIndex))

  assertEqual "user set to green after update" "green" =<< stringVariation client "flag-key-1" user "none"
  close client

testMultipleFlags :: Test
testMultipleFlags = TestCase $ do
  td <- TestData.newTestData
  TestData.update td =<<
      (TestData.flag td "flag-key-1"
          <&> TestData.variations [ toJSON ("blue" :: Text)
                                  , toJSON ("red" :: Text)
                                  , toJSON ("green" :: Text)
                                  ]
          <&> TestData.variationForAllUsers (0 :: TestData.VariationIndex))
  TestData.update td =<<
      (TestData.flag td "flag-key-2"
          <&> TestData.variationForAllUsers True)
  let config = testConfig (TestData.dataSourceFactory td)
  client <- makeClient config
  let user = makeContext "user" "user"
  assertEqual "flag 1" "blue" =<< stringVariation client "flag-key-1" user "none"
  assertEqual "flag 2" True =<< boolVariation client "flag-key-2" user False
  close client

testMultipleClients :: Test
testMultipleClients = TestCase $ do
  td <- TestData.newTestData
  let config = testConfig (TestData.dataSourceFactory td)
  client1 <- makeClient config
  client2 <- makeClient config
  let user = makeContext "user" "user"
  TestData.update td =<<
      (TestData.flag td "flag-key-1"
          <&> TestData.variations [ toJSON ("blue" :: Text)
                                  , toJSON ("red" :: Text)
                                  , toJSON ("green" :: Text)
                                  ]
          <&> TestData.variationForAllUsers (0 :: TestData.VariationIndex))
  assertEqual "client1 recieved update" "blue" =<< stringVariation client1 "flag-key-1" user "none"
  assertEqual "client2 recieved update" "blue" =<< stringVariation client2 "flag-key-1" user "none"
  close client2
  TestData.update td =<<
      (TestData.flag td "flag-key-1"
          <&> TestData.variationForAllUsers (2 :: TestData.VariationIndex))

  assertEqual "client1 recieved update to green" "green" =<< stringVariation client1 "flag-key-1" user "none"
  assertEqual "client2 no update after close" "none" =<< stringVariation client2 "flag-key-1" user "none"
  close client1

testTargeting :: Test
testTargeting = TestCase $ do
  td <- TestData.newTestData
  TestData.update td =<<
      (TestData.flag td "flag-key-1"
          <&> TestData.variations [ toJSON ("blue" :: Text)
                                  , toJSON ("red" :: Text)
                                  , toJSON ("green" :: Text)
                                  ]
          <&> TestData.variationForUser "ben" (0 :: TestData.VariationIndex)
          <&> TestData.variationForUser "todd" (0 :: TestData.VariationIndex)
          <&> TestData.offVariation (1 :: TestData.VariationIndex)
          <&> TestData.fallthroughVariation (2 :: TestData.VariationIndex))
  let config = testConfig (TestData.dataSourceFactory td)
      ben = makeContext "ben" "user"
      todd = makeContext "todd" "user"
      evelyn = makeContext "evelyn" "user"

  client <- makeClient config

  assertEqual "ben receives blue" "blue" =<< stringVariation client "flag-key-1" ben "none"
  assertEqual "todd receives blue" "blue" =<< stringVariation client "flag-key-1" todd "none"
  assertEqual "evelyn receives green" "green" =<< stringVariation client "flag-key-1" evelyn "none"
  TestData.update td =<<
      (TestData.flag td "flag-key-1"
          <&> TestData.on False)

  assertEqual "targeting off ben receives red" "red" =<< stringVariation client "flag-key-1" ben "none"
  assertEqual "targeting off todd receives red" "red" =<< stringVariation client "flag-key-1" todd "none"
  assertEqual "targeting off evelyn receives red" "red" =<< stringVariation client "flag-key-1" evelyn "none"

  close client


testRules :: Test
testRules = TestCase $ do
  td <- TestData.newTestData
  TestData.update td =<<
      (TestData.flag td "flag-key-1"
          <&> TestData.variations [ toJSON ("blue" :: Text)
                                  , toJSON ("red" :: Text)
                                  , toJSON ("green" :: Text)
                                  ]
          <&> TestData.ifMatch "country" [toJSON ("gb" :: Text), toJSON ("usa" :: Text)]
          <&> TestData.andMatch "name" [toJSON ("Todd" :: Text)]
          <&> TestData.thenReturn (1 :: TestData.VariationIndex)
          <&> TestData.ifNotMatch "name" [toJSON ("Todd" :: Text)]
          <&> TestData.andMatch "country" [toJSON ("gb" :: Text), toJSON ("usa" :: Text)]
          <&> TestData.thenReturn (2 :: TestData.VariationIndex)
          <&> TestData.fallthroughVariation (0 :: TestData.VariationIndex))
  let config = testConfig (TestData.dataSourceFactory td)
      ben = makeContext "ben" "user"
                & withAttribute "country" "usa"
                & withAttribute "name" "Ben"
      todd = makeContext "todd" "user"
                & withAttribute "country" "gb"
                & withAttribute "name" "Todd"
      evelyn = makeContext "evelyn" "user"

  client <- makeClient config

  assertEqual "ben receives green" "green" =<< stringVariation client "flag-key-1" ben "none"
  assertEqual "todd receives red" "red" =<< stringVariation client "flag-key-1" todd "none"
  assertEqual "evelyn receives blue" "blue" =<< stringVariation client "flag-key-1" evelyn "none"

  close client

data CustomType
    = CustomType1
    | CustomType2
    deriving (Generic, Eq, Show, ToJSON)

testValueForAllUsers :: Test
testValueForAllUsers = TestCase $ do
  td <- TestData.newTestData
  TestData.update td =<<
      (TestData.flag td "flag-key-1"
        <&> TestData.valueForAllUsers CustomType1)
  let config = testConfig (TestData.dataSourceFactory td)
      ben = makeContext "ben" "user"
      todd = makeContext "todd" "user"

  client <- makeClient config

  assertEqual "ben receives CustomType1" "CustomType1" =<< stringVariation client "flag-key-1" ben "CustomType2"
  assertEqual "todd receives CustomType1" "CustomType1" =<< stringVariation client "flag-key-1" todd "CustomType2"

  close client
