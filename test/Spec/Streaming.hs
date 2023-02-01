module Spec.Streaming (allTests) where

import Control.Monad (mzero)
import Data.Attoparsec.ByteString
import Test.HUnit

import LaunchDarkly.Server.Network.Streaming

allTests :: Test
allTests = TestList []
