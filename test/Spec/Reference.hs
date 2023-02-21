module Spec.Reference (allTests) where

import LaunchDarkly.Server.Reference (getComponents, getError, getRawPath, makeLiteral, makeReference)
import Test.HUnit

invalidReferences :: Test
invalidReferences =
    TestCase $
        ( do
            confirm "empty reference" ""
            confirm "empty reference" "/"

            confirm "trailing slash" "//"
            confirm "trailing slash" "/a/b/"
            confirm "double slash" "/a//b"

            confirm "invalid escape sequence" "/a~x"
            confirm "invalid escape sequence" "/a~"
            confirm "invalid escape sequence" "/a/b~x"
            confirm "invalid escape sequence" "/a/b~"
        )
  where
    confirm err reference = assertEqual "" err $ getError $ makeReference reference

validReferencesWithoutLeadingSlash :: Test
validReferencesWithoutLeadingSlash =
    TestCase $
        ( do
            confirm "key"
            confirm "kind"
            confirm "name"
            confirm "name/with/slashes"
            confirm "name~0~1with-what-looks-like-escape-sequences"
        )
  where
    confirm ref =
        let reference = makeReference ref
         in ( do
                assertEqual "" "" $ getError reference
                assertEqual "" ref $ getRawPath reference
                assertEqual "" [ref] $ getComponents reference
            )

validReferencesWithLeadingSlash :: Test
validReferencesWithLeadingSlash =
    TestCase $
        ( do
            confirm "/key" "key"
            confirm "/0" "0"
            confirm "/name~1with~1slashes~0and~0tildes" "name/with/slashes~and~tildes"
        )
  where
    confirm ref component =
        let reference = makeReference ref
         in ( do
                assertEqual "" "" $ getError reference
                assertEqual "" [component] $ getComponents reference
                assertEqual "" ref $ getRawPath reference
            )

canAccessSubcomponents :: Test
canAccessSubcomponents =
    TestCase $
        ( do
            confirm "/key" ["key"]
            confirm "/a/b" ["a", "b"]
            confirm "/a~1b/c" ["a/b", "c"]
            confirm "/a~0b/c" ["a~b", "c"]
            confirm "/a/10/20/30x" ["a", "10", "20", "30x"]

            confirm "" []
            confirm "key" ["key"]
            confirm "/key" ["key"]
            confirm "/a/b" ["a", "b"]
        )
  where
    confirm ref components =
        let reference = makeReference ref
         in assertEqual "" components $ getComponents reference

validLiteralReferences :: Test
validLiteralReferences =
    TestCase $
        ( do
            confirm "name" "name"
            confirm "a/b" "a/b"
            confirm "/a/b~c" "/~1a~1b~0c"
            confirm "/" "/~1"
        )
  where
    confirm ref path =
        let literal = makeLiteral ref
            reference = makeReference path
         in assertEqual "" (getRawPath literal) (getRawPath reference)

invalidLiteralReferences :: Test
invalidLiteralReferences = TestCase $ assertEqual "" "empty reference" $ getError $ makeLiteral ""

allTests :: Test
allTests =
    TestList
        [ invalidReferences
        , validReferencesWithoutLeadingSlash
        , validReferencesWithLeadingSlash
        , canAccessSubcomponents
        , validLiteralReferences
        , invalidLiteralReferences
        ]
