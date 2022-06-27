module Spec.User (allTests) where

import           GHC.Exts (fromList)
import           Test.HUnit
import           Data.Aeson
import           Data.Aeson.Types                  (Value(..))
import qualified Data.HashMap.Strict as            HM
import           Data.HashMap.Strict               (HashMap)
import           Data.Function                     ((&))

import           LaunchDarkly.Server.User
import           LaunchDarkly.Server.User.Internal

serializeEmpty :: Test
serializeEmpty = expected ~=? actual where
    actual = decode $ encode $ unwrapUser $ makeUser "abc"
    expected = pure $ Object $ fromList [("key", String "abc")]

allTests :: Test
allTests = TestList
    [ serializeEmpty
    ]
