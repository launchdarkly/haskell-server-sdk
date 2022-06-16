module Main where

import           Control.Monad (void)
import           Test.HUnit    (runTestTT, Test(TestList, TestLabel))

import qualified Spec.Bucket
import qualified Spec.Evaluate
import qualified Spec.Features
import qualified Spec.Operators
import qualified Spec.Redis
import qualified Spec.Segment
import qualified Spec.Store
import qualified Spec.StoreInterface
import qualified Spec.Redis
import qualified Spec.DataSource
import qualified Spec.Integrations.FileData
import qualified Spec.Streaming
import qualified Spec.User
import qualified Spec.Integrations.TestData

main :: IO ()
main = void $ runTestTT $ TestList
    [ Spec.Bucket.allTests
    , Spec.Evaluate.allTests
    , Spec.Features.allTests
    , Spec.Operators.allTests
    , Spec.Redis.allTests
    , Spec.Segment.allTests
    , Spec.Store.allTests
    , Spec.StoreInterface.allTests
    , Spec.DataSource.allTests
    , Spec.Redis.allTests
    , Spec.Streaming.allTests
    , Spec.User.allTests
    , TestLabel "Integration.FileData" Spec.Integrations.FileData.allTests
    , TestLabel "Integration.TestData" Spec.Integrations.TestData.allTests
    ]
