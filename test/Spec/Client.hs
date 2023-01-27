module Spec.Client (allTests) where

import           Test.HUnit
import           Data.Aeson                        (Value(..))
import qualified Data.HashMap.Strict as            HM
import           Data.HashMap.Strict               (HashMap)
import qualified Data.HashSet as                   HS
import           Data.Function                     ((&))
import           Data.Generics.Product             (getField)

import           LaunchDarkly.Server.Store
import           LaunchDarkly.Server.Store.Internal
import           LaunchDarkly.Server.Context
import           LaunchDarkly.Server.Client
import           LaunchDarkly.Server.Client.Internal
import           LaunchDarkly.Server.User
import           LaunchDarkly.Server.User.Internal
import           LaunchDarkly.Server.Features
import           LaunchDarkly.Server.Operators
import           LaunchDarkly.Server.Details
import           LaunchDarkly.Server.Store.Internal
import           LaunchDarkly.Server.Evaluate
import           LaunchDarkly.Server.Config

import           Util.Features
import LaunchDarkly.AesonCompat (fromList)
import LaunchDarkly.Server.Reference (makeReference, makeLiteral)
import Data.Text (Text)

makeEmptyStore :: IO (StoreHandle IO)
makeEmptyStore = do
    handle <- makeStoreIO Nothing 0
    initializeStore handle mempty mempty
    pure handle

testSecureModeHashIsGeneratedCorrectly :: Test
testSecureModeHashIsGeneratedCorrectly = TestCase $ do
    client@(Client clientI) <- makeTestClient "secret"
    assertEqual "" "aa747c502a898200f9e4fa21bac68136f886a0e27aec70ba06daf2e2a5cb5597" (secureModeHash client userContext)
    assertEqual "" "a045e65c6d23bda4559ed4f2371a2508ce63016ceff58b00aa07b435e2bfedaa" (secureModeHash client orgContext)

    where userContext = makeContext "Message" "user"
          orgContext = makeContext "Message" "org"

makeTestClient :: Text -> IO Client
makeTestClient sdkKey = do
    (Client client) <- makeClient $ (makeConfig sdkKey) & configSetOffline True
    initializeStore (getField @"store" client) mempty mempty
    pure (Client client)


allTests :: Test
allTests = TestList
    [ testSecureModeHashIsGeneratedCorrectly
    ]
