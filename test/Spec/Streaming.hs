module Spec.Streaming (allTests) where

import Test.HUnit
import Data.Attoparsec.ByteString
import Control.Monad (mzero)

import LaunchDarkly.Server.Network.Streaming

allTests :: Test
allTests = TestList []
