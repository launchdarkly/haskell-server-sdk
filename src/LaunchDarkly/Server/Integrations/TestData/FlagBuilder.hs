{-# LANGUAGE NamedFieldPuns #-}

module LaunchDarkly.Server.Integrations.TestData.FlagBuilder
    ( FlagBuilder (..)
    , VariationIndex
    , newFlagBuilder
    , booleanFlag
    , on
    , fallthroughVariation
    , offVariation
    , variationForAll
    , variationForAllUsers
    , valueForAll
    , valueForAllUsers
    , variationForKey
    , variationForUser
    , variations
    , buildFlag
    , ifMatch
    , ifMatchContext
    , ifNotMatch
    , ifNotMatchContext
    , FlagRuleBuilder
    , andMatch
    , andMatchContext
    , andNotMatch
    , andNotMatchContext
    , thenReturn
    , Variation
    )
where

import qualified Data.Aeson as Aeson
import Data.Function ((&))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Natural (Natural)
import qualified LaunchDarkly.Server.Features as F
import qualified LaunchDarkly.Server.Operators as Op
import LaunchDarkly.Server.Reference (makeReference)

type VariationIndex = Integer

trueVariationForBoolean, falseVariationForBoolean :: VariationIndex
trueVariationForBoolean = 0
falseVariationForBoolean = 1

variationForBoolean :: Bool -> VariationIndex
variationForBoolean True = trueVariationForBoolean
variationForBoolean False = falseVariationForBoolean

-- |
-- A builder for feature flag configurations to be used with
-- "LaunchDarkly.Server.Integrations.TestData".
--
-- see 'LaunchDarkly.Server.Integrations.TestData.flag' and
-- 'LaunchDarkly.Server.Integrations.TestData.update'
data FlagBuilder = FlagBuilder
    { fbKey :: Text
    , fbOffVariation :: Maybe VariationIndex
    , fbOn :: Bool
    , fbFallthroughVariation :: Maybe VariationIndex
    , fbVariations :: [Aeson.Value]
    , fbTargetMap :: Map Text (Map VariationIndex (HashSet Text))
    , fbRules :: [FlagRule]
    }
    deriving (Show)

fbTargets :: FlagBuilder -> ([F.Target], [F.Target])
fbTargets FlagBuilder {fbTargetMap = targetMap} =
    Map.foldlWithKey splitIntoTargets ([], []) targetMap
  where
    splitIntoTargets :: ([F.Target], [F.Target]) -> Text -> Map VariationIndex (HashSet Text) -> ([F.Target], [F.Target])
    splitIntoTargets acc "user" keyMap = Map.foldlWithKey foldForUserKind acc keyMap
    splitIntoTargets acc kind keyMap = Map.foldlWithKey (foldForOtherKind kind) acc keyMap

    -- When processing user kinds, we need to add a full target to the user targets, and a placeholder without the values in the context targets list
    foldForUserKind :: ([F.Target], [F.Target]) -> VariationIndex -> HashSet Text -> ([F.Target], [F.Target])
    foldForUserKind (userTargets, allTargets) variation values =
        ( F.Target {values, variation, contextKind = "user"} : userTargets
        , F.Target {values = mempty, variation, contextKind = "user"} : allTargets
        )

    foldForOtherKind :: Text -> ([F.Target], [F.Target]) -> VariationIndex -> HashSet Text -> ([F.Target], [F.Target])
    foldForOtherKind kind (userTargets, allTargets) variation values =
        ( userTargets
        , F.Target {values = values, variation, contextKind = kind} : allTargets
        )

buildFlag :: Natural -> FlagBuilder -> F.Flag
buildFlag version flagBuilder =
    F.Flag
        { F.key = fbKey flagBuilder
        , F.version = version
        , F.on = fbOn flagBuilder
        , F.trackEvents = False
        , F.trackEventsFallthrough = False
        , F.deleted = False
        , F.prerequisites = []
        , F.salt = "salt"
        , F.targets = userTargets
        , F.contextTargets = allTargets
        , F.rules = mapWithIndex convertFlagRule (fbRules flagBuilder)
        , F.fallthrough = F.VariationOrRollout (fbFallthroughVariation flagBuilder) Nothing
        , F.offVariation = fbOffVariation flagBuilder
        , F.variations = fbVariations flagBuilder
        , F.debugEventsUntilDate = Nothing
        , F.clientSideAvailability = F.ClientSideAvailability False False False
        }
  where
    (userTargets, allTargets) = fbTargets flagBuilder

mapWithIndex :: Integral num => (num -> a -> b) -> [a] -> [b]
mapWithIndex f l =
    fmap (uncurry f) (zip [0 ..] l)

newFlagBuilder :: Text -> FlagBuilder
newFlagBuilder key =
    FlagBuilder
        { fbKey = key
        , fbOffVariation = Nothing
        , fbOn = True
        , fbFallthroughVariation = Nothing
        , fbVariations = mempty
        , fbTargetMap = mempty
        , fbRules = mempty
        }

booleanFlagVariations :: [Aeson.Value]
booleanFlagVariations = [Aeson.Bool True, Aeson.Bool False]

isBooleanFlag :: FlagBuilder -> Bool
isBooleanFlag flagBuilder
    | booleanFlagVariations == fbVariations flagBuilder = True
    | otherwise = False

-- |
-- A shortcut for setting the flag to use the standard boolean configuration.
--
-- This is the default for all new flags created with
-- 'LaunchDarkly.Server.Integrations.TestData.flag'. The flag will have two
-- variations, @True@ and @False@ (in that order); it will return @False@
-- whenever targeting is off, and @True@ when targeting is on if no other
-- settings specify otherwise.
booleanFlag :: FlagBuilder -> FlagBuilder
booleanFlag flagBuilder
    | isBooleanFlag flagBuilder =
        flagBuilder
    | otherwise =
        flagBuilder
            & variations booleanFlagVariations
            & fallthroughVariation trueVariationForBoolean
            & offVariation falseVariationForBoolean

-- |
-- Sets targeting to be on or off for this flag.
--
-- The effect of this depends on the rest of the flag configuration, just as it
-- does on the real LaunchDarkly dashboard. In the default configuration that
-- you get from calling 'LaunchDarkly.Server.Integrations.TestData.flag' with a
-- new flag key, the flag will return @False@ whenever targeting is off, and
-- @True@ when targeting is on.
on ::
    -- | isOn @True@ if targeting should be on
    Bool ->
    FlagBuilder ->
    FlagBuilder
on isOn fb =
    fb {fbOn = isOn}

-- |
-- Removes any existing rules from the flag.
-- This undoes the effect of methods like 'ifMatch' or 'ifNotMatch'
clearRules :: FlagBuilder -> FlagBuilder
clearRules fb =
    fb {fbRules = mempty}

-- |
-- Removes any existing targets from the flag.
-- This undoes the effect of methods like 'variationForKey'
clearTargets :: FlagBuilder -> FlagBuilder
clearTargets fb =
    fb {fbTargetMap = mempty}

-- |
-- Sets the flag to always return the specified variation value for all
-- contexts.
--
-- The value may be of any type that implements 'Aeson.ToJSON'. This method
-- changes the flag to have only a single variation, which is this value, and
-- to return the same variation regardless of whether targeting is on or off.
-- Any existing targets or rules are removed.
valueForAll ::
    Aeson.ToJSON value =>
    value -> -- the desired value to be returned for all contexts
    FlagBuilder ->
    FlagBuilder
valueForAll val fb =
    fb
        & variations [Aeson.toJSON val]
        & variationForAll (0 :: VariationIndex)

{-# DEPRECATED valueForAllUsers "Use valueForAll instead" #-}

-- |
-- Sets the flag to always return the specified variation value for all users.
--
-- This function is an alias to 'valueForAll'.
--
-- The value may be of any type that implements 'Aeson.ToJSON'. This method
-- changes the flag to have only a single variation, which is this value, and
-- to return the same variation regardless of whether targeting is on or off.
-- Any existing targets or rules are removed.
valueForAllUsers ::
    Aeson.ToJSON value =>
    value -> -- the desired value to be returned for all users
    FlagBuilder ->
    FlagBuilder
valueForAllUsers = valueForAll

-- |
-- Changes the allowable variation values for the flag.
--
-- The value may be of any JSON type, as defined by 'Aeson.Value'. For
-- instance, a boolean flag normally has [toJSON True, toJSON False]; a
-- string-valued flag might have [toJSON "red", toJSON "green"]; etc.
variations ::
    -- | the desired variations
    [Aeson.Value] ->
    FlagBuilder ->
    FlagBuilder
variations values fb =
    fb {fbVariations = values}

-- Should this actually use overloaded function names?
class Variation val where
    -- |
    -- Specifies the fallthrough variation. The fallthrough is the value that
    -- is returned if targeting is on and the context was not matched by a more
    -- specific target or rule.
    --
    -- If the flag was previously configured with other variations and the
    -- variation specified is a boolean, this also changes it to a boolean
    -- flag.
    fallthroughVariation ::
        -- | @True@ or @False@ or the desired fallthrough variation index: 0 for the first, 1 for the second, etc.
        val ->
        FlagBuilder ->
        FlagBuilder

    -- |
    -- Specifies the off variation for a flag. This is the variation that is
    -- returned whenever targeting is off.
    --
    -- If the flag was previously configured with other variations and the
    -- variation specified is a boolean, this also changes it to a boolean
    -- flag.
    offVariation ::
        -- | @True@ or @False@ or the desired fallthrough variation index: 0 for the first, 1 for the second, etc.
        val ->
        FlagBuilder ->
        FlagBuilder

    -- |
    -- Sets the flag to always return the specified variation for all contexts.
    --
    -- The variation is specified, Targeting is switched on, and any existing
    -- targets or rules are removed. The fallthrough variation is set to the
    -- specified value. The off variation is left unchanged.
    --
    -- If the flag was previously configured with other variations and the
    -- variation specified is a boolean, this also changes it to a boolean
    -- flag.
    variationForAll ::
        -- | @True@ or @False@ or the desired fallthrough variation index: 0 for the first, 1 for the second, etc.
        val ->
        FlagBuilder ->
        FlagBuilder

    -- |
    -- Sets the flag to always return the specified variation for all users.
    --
    -- The variation is specified, Targeting is switched on, and any existing
    -- targets or rules are removed. The fallthrough variation is set to the
    -- specified value. The off variation is left unchanged.
    --
    -- If the flag was previously configured with other variations and the
    -- variation specified is a boolean, this also changes it to a boolean
    -- flag.
    variationForAllUsers ::
        -- | @True@ or @False@ or the desired fallthrough variation index: 0 for the first, 1 for the second, etc.
        val ->
        FlagBuilder ->
        FlagBuilder

    -- |
    -- Sets the flag to return the specified variation for a specific context
    -- kind and key when targeting is on.
    --
    -- This has no effect when targeting is turned off for the flag.
    --
    -- If the flag was previously configured with other variations and the
    -- variation specified is a boolean, this also changes it to a boolean
    -- flag.
    variationForKey ::
        -- | The context kind to match against
        Text ->
        -- | a key to target
        Text ->
        -- | @True@ or @False@ or the desired fallthrough variation index: 0 for the first, 1 for the second, etc.
        val ->
        FlagBuilder ->
        FlagBuilder

    -- |
    -- Sets the flag to return the specified variation for a specific user key
    -- when targeting is on.
    --
    -- This has no effect when targeting is turned off for the flag.
    --
    -- If the flag was previously configured with other variations and the
    -- variation specified is a boolean, this also changes it to a boolean
    -- flag.
    variationForUser ::
        -- | a user key to target
        Text ->
        -- | @True@ or @False@ or the desired fallthrough variation index: 0 for the first, 1 for the second, etc.
        val ->
        FlagBuilder ->
        FlagBuilder

    -- |
    -- Finishes defining the rule, specifying the result as either a boolean or
    -- a variation index.
    --
    -- If the flag was previously configured with other variations and the
    -- variation specified is a boolean, this also changes it to a boolean
    -- flag.
    thenReturn ::
        -- | @True@ or @False@ or the desired fallthrough variation index: 0 for the first, 1 for the second, etc.
        val ->
        FlagRuleBuilder ->
        FlagBuilder

instance Variation Integer where
    fallthroughVariation variationIndex fb =
        fb {fbFallthroughVariation = Just variationIndex}

    offVariation variationIndex fb =
        fb {fbOffVariation = Just variationIndex}

    variationForAll variationIndex fb =
        fb
            & on True
            & clearRules
            & clearTargets
            & fallthroughVariation variationIndex

    variationForAllUsers = variationForAll

    variationForKey kind key variationIndex fb@(FlagBuilder {fbTargetMap = targetMap}) =
        case Map.lookup kind targetMap of
            Nothing -> fb {fbTargetMap = Map.insert kind (Map.singleton variationIndex $ HS.singleton key) targetMap}
            Just m ->
                case Map.lookup variationIndex m of
                    Nothing -> fb {fbTargetMap = Map.insert kind (Map.insert variationIndex (HS.singleton key) m) targetMap}
                    Just keys -> fb {fbTargetMap = Map.insert kind (Map.insert variationIndex (HS.insert key keys) m) targetMap}

    variationForUser = variationForKey "user"

    thenReturn variationIndex ruleBuilder =
        let fb = frbBaseBuilder ruleBuilder
         in fb {fbRules = FlagRule (frbClauses ruleBuilder) variationIndex : fbRules fb}

instance Variation Bool where
    fallthroughVariation value fb =
        fb
            & booleanFlag
            & fallthroughVariation (variationForBoolean value)
    offVariation value fb =
        fb
            & booleanFlag
            & offVariation (variationForBoolean value)
    variationForAll value fb =
        fb
            & booleanFlag
            & variationForAll (variationForBoolean value)
    variationForAllUsers = variationForAll
    variationForKey kind key value fb =
        fb
            & booleanFlag
            & variationForKey kind key (variationForBoolean value)
    variationForUser userKey value fb =
        fb
            & booleanFlag
            & variationForUser userKey (variationForBoolean value)
    thenReturn value ruleBuilder =
        ruleBuilder {frbBaseBuilder = booleanFlag $ frbBaseBuilder ruleBuilder}
            & thenReturn (variationForBoolean value)

-- |
-- Starts defining a flag rule, using the "is one of" operator.
--
-- For example, this creates a rule that returns @True@ if the name is
-- \"Patsy\" or \"Edina\":
--
-- @
-- testData
--     & flag "flag"
--     & ifMatchContext "user" "name" [toJSON \"Patsy\", toJSON \"Edina\"]
--     & thenReturn True
-- @
ifMatchContext ::
    -- | the context kind to match again
    Text ->
    -- | the context attribute to match against
    Text ->
    -- | values to compare to
    [Aeson.Value] ->
    FlagBuilder ->
    -- | call 'thenReturn' to finish the rule, or add more tests with 'andMatch' or 'andNotMatch'
    FlagRuleBuilder
ifMatchContext kind attribute values fb =
    newFlagRuleBuilder fb
        & andMatchContext kind attribute values

{-# DEPRECATED ifMatch "Use ifMatchContext instead" #-}

-- |
-- Starts defining a flag rule, using the "is one of" operator.
--
-- This is a shortcut for calling 'ifMatch' with a context kind of "user".
--
-- For example, this creates a rule that returns @True@ if the name is
-- \"Patsy\" or \"Edina\":
--
-- @
-- testData
--     & flag "flag"
--     & ifMatch "name" [toJSON \"Patsy\", toJSON \"Edina\"]
--     & thenReturn True
-- @
ifMatch ::
    -- | the context attribute to match against
    Text ->
    -- | values to compare to
    [Aeson.Value] ->
    FlagBuilder ->
    -- | call 'thenReturn' to finish the rule, or add more tests with 'andMatch' or 'andNotMatch'
    FlagRuleBuilder
ifMatch = ifMatchContext "user"

-- |
-- Starts defining a flag rule, using the "is not one of" operator.
--
-- For example, this creates a rule that returns @True@ if the name is neither
-- \"Saffron\" nor \"Bubble\"
--
-- @
-- testData
--     & flag "flag"
--     & ifNotMatchContext "user" "name" [toJSON \"Saffron\", toJSON \"Bubble\"]
--     & thenReturn True
-- @
ifNotMatchContext ::
    -- | context kind to match again
    Text ->
    -- | attribute to match against
    Text ->
    -- | values to compare to
    [Aeson.Value] ->
    FlagBuilder ->
    -- | call 'thenReturn' to finish the rule, or add more tests with 'andMatch' or 'andNotMatch'
    FlagRuleBuilder
ifNotMatchContext kind attibute values fb =
    newFlagRuleBuilder fb
        & andNotMatchContext kind attibute values

{-# DEPRECATED ifNotMatch "Use ifNotMatchContext instead" #-}

-- |
-- Starts defining a flag rule, using the "is not one of" operator.
--
-- This is a shortcut for calling 'ifNotMatchContext' with a context kind of
-- "user".
--
-- For example, this creates a rule that returns @True@ if the name is neither
-- \"Saffron\" nor \"Bubble\"
--
-- @
-- testData
--     & flag "flag"
--     & ifNotMatch "name" [toJSON \"Saffron\", toJSON \"Bubble\"]
--     & thenReturn True
-- @
ifNotMatch ::
    -- | attribute to match against
    Text ->
    -- | values to compare to
    [Aeson.Value] ->
    FlagBuilder ->
    -- | call 'thenReturn' to finish the rule, or add more tests with 'andMatch' or 'andNotMatch'
    FlagRuleBuilder
ifNotMatch = ifNotMatchContext "user"

data Clause = Clause
    { clauseAttribute :: Text
    , contextKind :: Text
    , clauseValues :: [Aeson.Value]
    , clauseNegate :: Bool
    }
    deriving (Show)

data FlagRule = FlagRule
    { frClauses :: [Clause]
    , frVariation :: VariationIndex
    }
    deriving (Show)

convertFlagRule :: Integer -> FlagRule -> F.Rule
convertFlagRule idx flagRule =
    F.Rule
        { F.id = T.pack $ "rule" <> show idx
        , F.variationOrRollout = F.VariationOrRollout (Just $ frVariation flagRule) Nothing
        , F.clauses = fmap convertClause (frClauses flagRule)
        , F.trackEvents = False
        }

convertClause :: Clause -> F.Clause
convertClause clause =
    F.Clause
        { F.attribute = makeReference $ clauseAttribute clause
        , F.contextKind = contextKind clause
        , F.negate = clauseNegate clause
        , F.values = clauseValues clause
        , F.op = Op.OpIn
        }

-- |
-- A builder for feature flag rules to be used with 'FlagBuilder'.
--
-- In the LaunchDarkly model, a flag can have any number of rules, and a rule
-- can have any number of clauses. A clause is an individual test such as
-- \"name is \'X\'\". A rule matches a context if all of the rule's clauses
-- match the context.
--
-- To start defining a rule, use one of the matching functions such as
-- 'ifMatch' or 'ifNotMatch'. This defines the first clause for the rule.
--
-- Optionally, you may add more clauses with the rule builder functions such as
-- 'andMatch' and 'andNotMatch'.
--
-- Finally, call 'thenReturn' to finish defining the rule.
data FlagRuleBuilder = FlagRuleBuilder
    { frbClauses :: [Clause]
    , frbBaseBuilder :: FlagBuilder
    }
    deriving (Show)

newFlagRuleBuilder :: FlagBuilder -> FlagRuleBuilder
newFlagRuleBuilder baseBuilder =
    FlagRuleBuilder
        { frbClauses = mempty
        , frbBaseBuilder = baseBuilder
        }

-- |
-- Adds another clause, using the "is one of" operator.
--
-- For example, this creates a rule that returns @True@ if the name is
-- \"Patsy\" and the country is \"gb\":
--
-- @
-- testData
--     & flag "flag"
--     & ifMatch "name" [toJSON \"Patsy\"]
--     & andMatch "country" [toJSON \"gb\"]
--     & thenReturn True
-- @
andMatchContext ::
    -- | the context kind to match again
    Text ->
    -- | the context attribute to match against
    Text ->
    -- | values to compare to
    [Aeson.Value] ->
    FlagRuleBuilder ->
    FlagRuleBuilder
andMatchContext kind attribute values ruleBuilder =
    ruleBuilder {frbClauses = Clause attribute kind values False : frbClauses ruleBuilder}

{-# DEPRECATED andMatch "Use andMatchContext instead" #-}

-- |
-- Adds another clause, using the "is one of" operator.
--
-- This is a shortcut for calling 'andMatchContext' with a context kind of
-- "user".
--
-- For example, this creates a rule that returns @True@ if the name is
-- \"Patsy\" and the country is \"gb\":
--
-- @
-- testData
--     & flag "flag"
--     & ifMatch "name" [toJSON \"Patsy\"]
--     & andMatch "country" [toJSON \"gb\"]
--     & thenReturn True
-- @
andMatch ::
    -- | the context attribute to match against
    Text ->
    -- | values to compare to
    [Aeson.Value] ->
    FlagRuleBuilder ->
    FlagRuleBuilder
andMatch = andMatchContext "user"

-- |
-- Adds another clause, using the "is not one of" operator.
--
-- For example, this creates a rule that returns @True@ if the name is
-- \"Patsy\" and the country is not \"gb\":
--
-- @
-- testData
--     & flag "flag"
--     & ifMatchContext "user" "name" [toJSON \"Patsy\"]
--     & andNotMatchContext "user" "country" [toJSON \"gb\"]
--     & thenReturn True
-- @
andNotMatchContext ::
    -- | the context kind to match against
    Text ->
    -- | the context attribute to match against
    Text ->
    -- | values to compare to
    [Aeson.Value] ->
    FlagRuleBuilder ->
    FlagRuleBuilder
andNotMatchContext kind attribute values ruleBuilder =
    ruleBuilder {frbClauses = Clause attribute kind values True : frbClauses ruleBuilder}

{-# DEPRECATED andNotMatch "Use andNotMatchContext instead" #-}

-- |
-- Adds another clause, using the "is not one of" operator.
--
-- This is a shortcut for calling 'andNotMatchContext' with a context kind of
-- "user".
--
-- For example, this creates a rule that returns @True@ if the name is
-- \"Patsy\" and the country is not \"gb\":
--
-- @
-- testData
--     & flag "flag"
--     & ifMatch "name" [toJSON \"Patsy\"]
--     & andNotMatch "country" [toJSON \"gb\"]
--     & thenReturn True
-- @
andNotMatch ::
    -- | the context attribute to match against
    Text ->
    -- | values to compare to
    [Aeson.Value] ->
    FlagRuleBuilder ->
    FlagRuleBuilder
andNotMatch = andNotMatchContext "user"

{-# DEPRECATED variationForAllUsers "Use variationForAll instead" #-}
{-# DEPRECATED variationForUser "Use variationForKey instead" #-}
