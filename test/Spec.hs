module Main where

import           Control.Monad (void)
import           Test.HUnit    (runTestTT, Test(TestList, TestLabel), Counts(..))

import qualified Spec.Bucket
import qualified Spec.Client
import qualified Spec.Config
import qualified Spec.Context
import qualified Spec.Evaluate
import qualified Spec.Features
import qualified Spec.Operators
import qualified Spec.Reference
import qualified Spec.Redis
import qualified Spec.Segment
import qualified Spec.Store
import qualified Spec.StoreInterface
import qualified Spec.DataSource
import qualified Spec.Integrations.FileData
import qualified Spec.Streaming
import qualified Spec.User
import qualified Spec.Integrations.TestData
import Control.Monad.Cont (when)
import System.Exit (exitWith, ExitCode (ExitFailure))

main :: IO ()
main = do
    Counts {..} <- runTestTT $ TestList
        [ Spec.Bucket.allTests
        , Spec.Client.allTests
        , Spec.Config.allTests
        , Spec.Context.allTests
        , Spec.DataSource.allTests
        , Spec.Evaluate.allTests
        , Spec.Features.allTests
        , Spec.Operators.allTests
        , Spec.Redis.allTests
        , Spec.Reference.allTests
        , Spec.Segment.allTests
        , Spec.Store.allTests
        , Spec.StoreInterface.allTests
        , Spec.Streaming.allTests
        , Spec.User.allTests
        , TestLabel "Integration.FileData" Spec.Integrations.FileData.allTests
        , TestLabel "Integration.TestData" Spec.Integrations.TestData.allTests
        ]
    when (errors + failures > 0) $ exitWith (ExitFailure 1)
