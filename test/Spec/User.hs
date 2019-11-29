module Spec.User (allTests) where

import Test.HUnit
import Data.Aeson
import Data.Aeson.Types (Value(..))
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Function ((&))

import LaunchDarkly.Server.User

serializeEmpty :: Test
serializeEmpty = expected ~=? actual where
    actual = decode $ encode $ makeUser "abc"
    expected = pure $ Object $ HM.fromList [("key", String "abc")]

allTests :: Test
allTests = TestList
    [ serializeEmpty
    ]
