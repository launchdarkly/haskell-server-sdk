module Spec.DataSource (allTests) where

import           Test.HUnit
import LaunchDarkly.Server.Client
import LaunchDarkly.Server.Config
import LaunchDarkly.Server.DataSource.Internal
import Data.Generics.Product               (getField)

allTests :: Test 
allTests =
    TestList []
