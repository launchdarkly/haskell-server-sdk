module Main where

import           Control.Monad (void)
import           Test.HUnit    (runTestTT, Test(TestList))

import qualified Spec.Operators
import qualified Spec.Segment
import qualified Spec.Bucket
import qualified Spec.Streaming
import qualified Spec.User
import qualified Spec.Evaluate
import qualified Spec.Store
import qualified Spec.StoreInterface

main :: IO ()
main = void $ runTestTT $ TestList
    [ Spec.Operators.allTests
    , Spec.Bucket.allTests
    , Spec.Segment.allTests
    , Spec.Streaming.allTests
    , Spec.User.allTests
    , Spec.Evaluate.allTests
    , Spec.Store.allTests
    , Spec.StoreInterface.allTests
    ]
