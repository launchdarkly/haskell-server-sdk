module Spec.DataSource (allTests) where

import Data.Generics.Product (getField)
import LaunchDarkly.Server.Client
import LaunchDarkly.Server.Config
import LaunchDarkly.Server.DataSource.Internal
import Test.HUnit

allTests :: Test
allTests =
    TestList []
