module Spec.Operators (allTests) where

import Test.HUnit
import Data.Aeson.Types (Value(..))

import LaunchDarkly.Server.Operators

dateStr1 :: Value
dateStr1 = String "2017-12-06T00:00:00.000-07:00"

dateStr2 :: Value
dateStr2 = String "2017-12-06T00:01:01.000-07:00"

invalidDate :: Value
invalidDate = String "hey what's this?"

dateMs1 :: Value
dateMs1 = Number $ fromInteger 10000000

dateMs2 :: Value
dateMs2 = Number $ fromInteger 10000001

makeTest :: (Op, Value, Value, Bool) -> Test
makeTest f@(o, x, y, e) = TestCase $ assertEqual (show f) e $ (getOperation o) x y

allTests :: Test
allTests = TestList $ map makeTest
    -- numeric operators
    [ (OpIn, Number $ fromInteger 50, Number $ fromInteger 50, True)
    , (OpIn, Number $ fromRational 99.0001, Number $ fromRational 99.0001, True)
    , (OpLessThan, Number $ fromInteger 1, Number $ fromRational 1.99999, True)
    , (OpLessThan, Number $ fromRational 1.99999, Number $ fromInteger 1, False)
    , (OpLessThan, Number $ fromInteger 1, Number $ fromInteger 2, True)
    , (OpLessThanOrEqual, Number $ fromInteger 1, Number $ fromInteger 1, True)
    , (OpGreaterThan, Number $ fromInteger 2, Number $ fromRational 1.99999, True)
    , (OpGreaterThan, Number $ fromRational 1.99999, Number $ fromInteger 2, False)
    , (OpGreaterThan, Number $ fromInteger 2, Number $ fromInteger 1, True)
    , (OpGreaterThanOrEqual, Number $ fromInteger 1, Number $ fromInteger 1, True)
    -- string operators
    , (OpIn, String "x", String "x", True)
    , (OpIn, String "x", String "xyz", False)
    , (OpStartsWith, String "xyz", String "x", True)
    , (OpStartsWith, String "x", String "xyz", False)
    , (OpEndsWith, String "xyz", String "z", True)
    , (OpEndsWith, String "z", String "xyz", False)
    , (OpContains, String "xyz", String "y", True)
    , (OpContains, String "y", String "yz", False)
    -- mixed strings and numbers
    , (OpIn, String "99", Number $ fromInteger 99, False)
    , (OpIn, Number $ fromInteger 99, String "99", False)
    , (OpContains, String "99", Number $ fromInteger 99, False)
    , (OpStartsWith, String "99", Number $ fromInteger 99, False)
    , (OpEndsWith, String "99", Number $ fromInteger 99, False)
    , (OpLessThanOrEqual, String "99", Number $ fromInteger 99, False)
    , (OpLessThanOrEqual, Number $ fromInteger 99, String "99", False)
    , (OpGreaterThanOrEqual, String "99", Number $ fromInteger 99, False)
    , (OpGreaterThanOrEqual, Number $ fromInteger 99, String "99", False)
    -- date operators
    , (OpBefore, dateStr1, dateStr2, True)
    , (OpBefore, dateMs1, dateMs2, True)
    , (OpBefore, dateStr2, dateStr1, False)
    , (OpBefore, dateMs2, dateMs1, False)
    , (OpBefore, dateStr1, dateStr1, False)
    , (OpBefore, dateMs1, dateMs1, False)
    , (OpBefore, String "", dateStr1, False)
    , (OpBefore, dateStr1, invalidDate, False)
    , (OpAfter, dateStr2, dateStr1, True)
    , (OpAfter, dateMs2, dateMs1, True)
    , (OpAfter, dateStr1, dateStr2, False)
    , (OpAfter, dateMs1, dateMs2, False)
    , (OpAfter, dateStr1, dateStr1, False)
    , (OpAfter, dateMs1, dateMs1, False)
    , (OpAfter, String "", dateStr1, False)
    , (OpAfter, dateStr1, invalidDate, False)
    -- regex
    , (OpMatches, String "hello world", String "hello.*rld", True)
    , (OpMatches, String "hello world", String "hello.*orl", True)
    , (OpMatches, String "hello world", String "l+", True)
    , (OpMatches, String "hello world", String "(world|planet)", True)
    , (OpMatches, String "hello world", String "aloha", False)
    , (OpMatches, String "hello world", String "***bad rg", False)
    -- semver operators
    , (OpSemVerEqual, String "2.0.0", String "2.0.0", True)
    , (OpSemVerEqual, String "2.0", String "2.0.0", True)
    , (OpSemVerEqual, String "2.0-rc1", String "2.0.0-rc1", True)
    , (OpSemVerEqual, String "2+build2", String "2.0.0+build2", True)
    , (OpSemVerEqual, String "2.0.0", String "2.0.1", False)
    , (OpSemVerEqual, String "02.0.0", String "2.0.0", False)
    , (OpSemVerLessThan, String "2.0.0", String "2.0.1", True)
    , (OpSemVerLessThan, String "2.0", String "2.0.1", True)
    , (OpSemVerLessThan, String "2.0.1", String "2.0.0", False)
    , (OpSemVerLessThan, String "2.0.1", String "2.0", False)
    , (OpSemVerLessThan, String "2.0.1", String "xbad%ver", False)
    , (OpSemVerLessThan, String "2.0.0-rc", String "2.0.0-rc.beta", True)
    , (OpSemVerGreaterThan, String "2.0.1", String "2.0", True)
    , (OpSemVerGreaterThan, String "2.0.0", String "2.0.1", False)
    , (OpSemVerGreaterThan, String "2.0", String "2.0.1", False)
    , (OpSemVerGreaterThan, String "2.0.1", String "xbad%ver", False)
    , (OpSemVerGreaterThan, String "2.0.0-rc.1", String "2.0.0-rc.0", True)
    ]
