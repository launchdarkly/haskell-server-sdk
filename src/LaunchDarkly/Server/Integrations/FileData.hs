{-# LANGUAGE BangPatterns #-}

-- | Integration between the LaunchDarkly SDK and file data.
--
--  The file data source allows you to use local files as a source of feature flag state. This would
--  typically be used in a test environment, to operate using a predetermined feature flag state
--  without an actual LaunchDarkly connection. See 'dataSourceFactory' for details.
--
--  @since 2.2.1
module LaunchDarkly.Server.Integrations.FileData
    ( dataSourceFactory
    )
where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, Value, decode)
import qualified Data.ByteString.Lazy as BSL
import Data.Generics.Product (getField)
import Data.HashSet (HashSet)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import LaunchDarkly.AesonCompat (KeyMap, mapWithKey)
import LaunchDarkly.Server.Client.Status
import LaunchDarkly.Server.DataSource.Internal (DataSource (..), DataSourceFactory, DataSourceUpdates (..))
import qualified LaunchDarkly.Server.Features as F

data FileFlag = FileFlag
    { version :: Maybe Natural
    , on :: Maybe Bool
    , targets :: Maybe [F.Target]
    , contextTargets :: Maybe [F.Target]
    , rules :: Maybe [F.Rule]
    , fallthrough :: Maybe F.VariationOrRollout
    , offVariation :: Maybe Integer
    , variations :: ![Value]
    }
    deriving (Generic, FromJSON, Show, Eq)

expandSimpleFlag :: Value -> FileFlag
expandSimpleFlag value =
    FileFlag
        { version = Nothing
        , on = Nothing
        , targets = Nothing
        , contextTargets = Nothing
        , rules = Nothing
        , fallthrough = Just (F.VariationOrRollout (Just 0) Nothing)
        , offVariation = Just 0
        , variations = [value]
        }

fromFileFlag :: Text -> FileFlag -> F.Flag
fromFileFlag key fileFlag =
    F.Flag
        { F.key = key
        , F.version = fromMaybe 1 $ getField @"version" fileFlag
        , F.on = fromMaybe True $ on fileFlag
        , F.trackEvents = False
        , F.trackEventsFallthrough = False
        , F.deleted = False
        , F.prerequisites = []
        , F.salt = ""
        , F.targets = fromMaybe [] $ targets fileFlag
        , F.contextTargets = fromMaybe [] $ contextTargets fileFlag
        , F.rules = fromMaybe [] $ getField @"rules" fileFlag
        , F.fallthrough = fromMaybe noFallthrough $ fallthrough fileFlag
        , F.offVariation = offVariation fileFlag
        , F.variations = variations fileFlag
        , F.debugEventsUntilDate = Nothing
        , F.clientSideAvailability = F.ClientSideAvailability False False False
        }

noFallthrough :: F.VariationOrRollout
noFallthrough =
    F.VariationOrRollout Nothing Nothing

data FileSegment = FileSegment
    { included :: Maybe (HashSet Text)
    , includedContexts :: Maybe [F.SegmentTarget]
    , excluded :: Maybe (HashSet Text)
    , excludedContexts :: Maybe [F.SegmentTarget]
    , rules :: Maybe [F.SegmentRule]
    , version :: Maybe Natural
    }
    deriving (Generic, FromJSON, Show, Eq)

fromFileSegment :: Text -> FileSegment -> F.Segment
fromFileSegment key fileSegment =
    F.Segment
        { F.key = key
        , F.version = fromMaybe 1 $ getField @"version" fileSegment
        , F.included = fromMaybe mempty $ included fileSegment
        , F.includedContexts = fromMaybe mempty $ includedContexts fileSegment
        , F.excluded = fromMaybe mempty $ excluded fileSegment
        , F.excludedContexts = fromMaybe mempty $ excludedContexts fileSegment
        , F.salt = ""
        , F.rules = fromMaybe [] $ getField @"rules" fileSegment
        , F.deleted = False
        }

data FileBody = FileBody
    { flags :: Maybe (KeyMap FileFlag)
    , flagValues :: Maybe (KeyMap Value)
    , segments :: Maybe (KeyMap FileSegment)
    }
    deriving (Generic, Show, FromJSON)

instance Semigroup FileBody where
    f1 <> f2 =
        FileBody
            { flags = flags f1 <> flags f2
            , flagValues = flagValues f1 <> flagValues f2
            , segments = segments f1 <> segments f2
            }
instance Monoid FileBody where
    mempty =
        FileBody
            { flags = mempty
            , flagValues = mempty
            , segments = mempty
            }
    mappend = (<>)

-- |
-- Creates a @DataSourceFactory@ which uses the configured the file data sources.
-- This allows you to use local files as a source of
-- feature flag state, instead of using an actual LaunchDarkly connection.
--
-- To use the file dataSource you can add it to the 'LaunchDarkly.Server.Config' using 'LaunchDarkly.Server.Config.configSetDataSourceFactory'
--
-- @
-- let config = configSetDataSourceFactory (FileData.dataSourceFactory ["./testData/flags.json"]) $
--              makeConfig "sdk-key"
-- client <- makeClient config
-- @
--
-- This will cause the client /not/ to connect to LaunchDarkly to get feature flags. The
-- client may still make network connections to send analytics events, unless you have disabled
-- this with 'LaunchDarkly.Server.Config.configSetSendEvents' to @False@.
-- IMPORTANT: Do /not/ set 'LaunchDarkly.Server.Config.configSetOffline' to @True@; doing so
-- would not just put the SDK \"offline\" with regard to LaunchDarkly, but will completely turn off
-- all flag data sources to the SDK /including the file data source/.
--
-- Flag data files can be either JSON or YAML. They contain an object with three possible
-- properties:
--
--      [@flags@]: Feature flag definitions.
--      [@flagValues@]: Simplified feature flags that contain only a value.
--      [@segments@]: Context segment definitions.
--
-- The format of the data in @flags@ and @segments@ is defined by the LaunchDarkly application
-- and is subject to change. Rather than trying to construct these objects yourself, it is simpler
-- to request existing flags directly from the LaunchDarkly server in JSON format, and use this
-- output as the starting point for your file. In Linux you would do this:
--
-- @
-- curl -H "Authorization: {your sdk key}" https://app.launchdarkly.com/sdk/latest-all
-- @
--
-- The output will look something like this (but with many more properties):
--
-- @
-- {
--     "flags": {
--         "flag-key-1": {
--             "key": "flag-key-1",
--             "on": true,
--             "variations": [ "a", "b" ]
--         },
--         "flag-key-2": {
--             "key": "flag-key-2",
--             "on": true,
--             "variations": [ "c", "d" ]
--         }
--     },
--     "segments": {
--         "segment-key-1": {
--             "key": "segment-key-1",
--             "includes": [ "user-key-1" ]
--         }
--     }
-- }
-- @
--
-- Data in this format allows the SDK to exactly duplicate all the kinds of flag behavior supported
-- by LaunchDarkly. However, in many cases you will not need this complexity, but will just want to
-- set specific flag keys to specific values. For that, you can use a much simpler format:
--
-- @
-- {
--     "flagValues": {
--         "my-string-flag-key": "value-1",
--         "my-boolean-flag-key": true,
--         "my-integer-flag-key": 3
--     }
-- }
-- @
--
-- Or, in YAML:
--
-- @
-- flagValues:
--   my-string-flag-key: "value-1"
--   my-boolean-flag-key: true
-- @
--
-- It is also possible to specify both @flags@ and @flagValues@, if you want some flags
-- to have simple values and others to have complex behavior. However, it is an error to use the
-- same flag key or segment key more than once, either in a single file or across multiple files.
--
-- If the data source encounters any error in any file(malformed content, a missing file) it will not load flags from that file.
-- If the data source encounters a duplicate key it will ignore that duplicate entry.
--
-- @since 2.2.1
dataSourceFactory :: [FilePath] -> DataSourceFactory
dataSourceFactory sources _clientContext dataSourceUpdates = do
    inited <- newIORef False
    let dataSourceIsInitialized =
            readIORef inited
        dataSourceStart = do
            FileBody mFlags mFlagValues mSegments <- mconcat <$> traverse loadFile sources
            let mSimpleFlags = fmap (fmap expandSimpleFlag) mFlagValues
                flags' = maybe mempty (mapWithKey fromFileFlag) (mFlags <> mSimpleFlags)
                segments' = maybe mempty (mapWithKey fromFileSegment) mSegments
            _ <- dataSourceUpdatesInit dataSourceUpdates flags' segments'
            dataSourceUpdatesSetStatus dataSourceUpdates Initialized
            writeIORef inited True
        dataSourceStop = pure ()
    pure $ DataSource {..}

loadFile :: FilePath -> IO FileBody
loadFile filePath = do
    file <- BSL.readFile filePath
    let mDecodedFile = decode file <|> Yaml.decodeThrow (BSL.toStrict file)
    case mDecodedFile of
        Just !fileBody ->
            pure fileBody
        Nothing ->
            pure mempty
