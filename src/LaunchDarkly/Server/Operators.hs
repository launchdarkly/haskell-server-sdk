module LaunchDarkly.Server.Operators
    ( Op(..)
    , getOperation
    ) where

import Data.Maybe            (fromMaybe, isJust)
import Data.Either           (fromRight)
import Data.Text as          T
import Data.Text             (Text, isPrefixOf, isInfixOf, isSuffixOf, unpack)
import Data.Char             (isDigit)
import Data.Text.Encoding    (encodeUtf8)
import Data.Scientific       (Scientific, toRealFloat)
import Data.Aeson.Types      (Value(..), FromJSON, ToJSON(..), withText, parseJSON)
import Data.Time.ISO8601     (parseISO8601)
import Data.Time.Clock       (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.SemVer           (Version, fromText, toText, metadata)
import Control.Monad         (liftM2)
import Control.Lens          ((.~))
import GHC.Generics          (Generic)
import Text.Regex.PCRE.Light (compileM, match)

data Op =
      OpIn
    | OpEndsWith
    | OpStartsWith
    | OpMatches
    | OpContains
    | OpLessThan
    | OpLessThanOrEqual
    | OpGreaterThan
    | OpGreaterThanOrEqual
    | OpBefore
    | OpAfter
    | OpSemVerEqual
    | OpSemVerLessThan
    | OpSemVerGreaterThan
    | OpSegmentMatch
    | OpUnknown
    deriving (Generic, Show, Eq)

instance FromJSON Op where
    parseJSON = withText "Op" $ \v -> case v of
        "in"                 -> pure OpIn
        "endsWith"           -> pure OpEndsWith
        "startsWith"         -> pure OpStartsWith
        "matches"            -> pure OpMatches
        "contains"           -> pure OpContains
        "lessThan"           -> pure OpLessThan
        "lessThanOrEqual"    -> pure OpLessThanOrEqual
        "greaterThan"        -> pure OpGreaterThan
        "greaterThanOrEqual" -> pure OpGreaterThanOrEqual
        "before"             -> pure OpBefore
        "after"              -> pure OpAfter
        "semVerEqual"        -> pure OpSemVerEqual
        "semVerLessThan"     -> pure OpSemVerLessThan
        "semVerGreaterThan"  -> pure OpSemVerGreaterThan
        "segmentMatch"       -> pure OpSegmentMatch
        _                    -> pure OpUnknown

instance ToJSON Op where
    toJSON op = String $ case op of
        OpIn                 -> "in"
        OpEndsWith           -> "endsWith"
        OpStartsWith         -> "startsWith"
        OpMatches            -> "matches"
        OpContains           -> "contains"
        OpLessThan           -> "lessThan"
        OpLessThanOrEqual    -> "lessThanOrEqual"
        OpGreaterThan        -> "greaterThan"
        OpGreaterThanOrEqual -> "greaterThanOrEqual"
        OpBefore             -> "before"
        OpAfter              -> "after"
        OpSemVerEqual        -> "semVerEqual"
        OpSemVerLessThan     -> "semVerLessThan"
        OpSemVerGreaterThan  -> "semVerGreaterThan"
        OpSegmentMatch       -> "segmentMatch"
        OpUnknown            -> "unknown"

checkString :: (Text -> Text -> Bool) -> Value -> Value -> Bool
checkString op (String x) (String y) = op x y
checkString _ _ _                    = False

checkNumber :: (Scientific -> Scientific -> Bool) -> Value -> Value -> Bool
checkNumber op (Number x) (Number y) = op x y
checkNumber _ _ _                    = False

doubleToPOSIXTime :: Double -> POSIXTime
doubleToPOSIXTime = realToFrac

parseTime :: Value -> Maybe UTCTime
parseTime (Number x) = Just $ posixSecondsToUTCTime $ doubleToPOSIXTime $ (toRealFloat x) / 1000
parseTime (String x) = parseISO8601 $ unpack x
parseTime _          = Nothing

compareTime :: (UTCTime -> UTCTime -> Bool) -> Value -> Value -> Bool
compareTime op x y = fromMaybe False $ liftM2 op (parseTime x) (parseTime y)

padSemVer :: Text -> Text
padSemVer text = T.concat [l, padding, r] where
    (l, r) = T.span (\c -> isDigit c || c == '.') text
    dots = T.count "." l
    padding = if dots < 2 then T.replicate (2 - dots) ".0" else ""

parseSemVer :: Text -> Either String Version
parseSemVer raw = fmap (metadata .~ []) (fromText $ padSemVer raw) >>= \x ->
    if T.isPrefixOf (toText x) (padSemVer raw) then Right x else Left "mismatch" where

compareSemVer :: (Version -> Version -> Bool) -> Text -> Text -> Bool
compareSemVer op x y = fromRight False $ liftM2 op (parseSemVer x) (parseSemVer y)

matches :: Text -> Text -> Bool
matches text pattern = case compileM (encodeUtf8 pattern) [] of
    Left _         -> False
    Right compiled -> isJust $ match compiled (encodeUtf8 text) []

getOperation :: Op -> (Value -> Value -> Bool)
getOperation op = case op of
    OpIn                 -> (==)
    OpEndsWith           -> checkString (flip isSuffixOf)
    OpStartsWith         -> checkString (flip isPrefixOf)
    OpContains           -> checkString (flip isInfixOf)
    OpMatches            -> checkString matches
    OpLessThan           -> checkNumber (<)
    OpLessThanOrEqual    -> checkNumber (<=)
    OpGreaterThan        -> checkNumber (>)
    OpGreaterThanOrEqual -> checkNumber (>=)
    OpBefore             -> compareTime (<)
    OpAfter              -> compareTime (>)
    OpSemVerEqual        -> checkString $ compareSemVer (==)
    OpSemVerLessThan     -> checkString $ compareSemVer (<)
    OpSemVerGreaterThan  -> checkString $ compareSemVer (>)
    OpSegmentMatch       -> error "cannot get operation for OpSegmentMatch"
    OpUnknown            -> const $ const False
