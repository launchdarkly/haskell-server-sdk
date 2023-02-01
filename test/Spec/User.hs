module Spec.User (allTests) where

import Data.Aeson
import Data.Aeson.Types (Value (..))
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import GHC.Exts (fromList)
import Test.HUnit

import LaunchDarkly.Server.User
import LaunchDarkly.Server.User.Internal

serializeEmpty :: Test
serializeEmpty = expected ~=? actual
  where
    actual = decode $ encode $ unwrapUser $ makeUser "abc"
    expected = pure $ Object $ fromList [("key", String "abc")]

allTests :: Test
allTests =
    TestList
        [ serializeEmpty
        ]
