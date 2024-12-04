{-# LANGUAGE NumericUnderscores #-}

module Utils where

import Control.Concurrent (threadDelay)
import Control.Lens ((&))
import Data.Generics.Product (getField)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import GHC.Natural (Natural, quotNatural)
import qualified LaunchDarkly.Server as LD
import qualified LaunchDarkly.Server.Reference as R
import Types

createClient :: CreateClientParams -> IO LD.Client
createClient p = LD.makeClient $ createConfig $ getField @"configuration" p

waitClient :: LD.Client -> IO ()
waitClient client = do
    status <- LD.getStatus client
    case status of
        LD.Initialized -> return ()
        _ -> threadDelay (1 * 1_000) >> waitClient client

createConfig :: ConfigurationParams -> LD.Config
createConfig p =
    LD.makeConfig (getField @"credential" p)
        & streamingConfig (getField @"streaming" p)
        & pollingConfig (getField @"polling" p)
        & tagsConfig (getField @"tags" p)
        & eventConfig (getField @"events" p)

updateConfig :: (a -> LD.Config -> LD.Config) -> Maybe a -> LD.Config -> LD.Config
updateConfig f Nothing config = config
updateConfig f (Just x) config = f x config

streamingConfig :: Maybe StreamingParams -> LD.Config -> LD.Config
streamingConfig Nothing c = c
streamingConfig (Just p) c =
    updateConfig LD.configSetStreamURI (getField @"baseUri" p) $
        updateConfig LD.configSetInitialRetryDelay (getField @"initialRetryDelayMs" p) c

pollingConfig :: Maybe PollingParams -> LD.Config -> LD.Config
pollingConfig Nothing c = c
pollingConfig (Just p) c =
    updateConfig LD.configSetBaseURI (getField @"baseUri" p) $
        updateConfig LD.configSetStreaming (Just False) $
            updateConfig LD.configSetPollIntervalSeconds ((`quotNatural` 1_000) <$> getField @"pollIntervalMs" p) c

tagsConfig :: Maybe TagParams -> LD.Config -> LD.Config
tagsConfig Nothing c = c
tagsConfig (Just params) c = LD.configSetApplicationInfo appInfo c
  where
    appInfo =
        LD.makeApplicationInfo
            & setApplicationInfo "id" (getField @"applicationId" params)
            & setApplicationInfo "version" (getField @"applicationVersion" params)

setApplicationInfo :: Text -> Maybe Text -> LD.ApplicationInfo -> LD.ApplicationInfo
setApplicationInfo _ Nothing appInfo = appInfo
setApplicationInfo key (Just value) appInfo = LD.withApplicationValue key value appInfo

eventConfig :: Maybe EventParams -> LD.Config -> LD.Config
eventConfig Nothing c = updateConfig LD.configSetSendEvents (Just False) c
eventConfig (Just p) c =
    updateConfig LD.configSetEventsURI (getField @"baseUri" p) $
        updateConfig LD.configSetEventsCapacity (getField @"capacity" p) $
            updateConfig LD.configSetCompressEvents (getField @"enableGzip" p) $
                updateConfig LD.configSetAllAttributesPrivate (getField @"allAttributesPrivate" p) $
                    updateConfig LD.configSetPrivateAttributeNames ((S.map R.makeReference) <$> getField @"globalPrivateAttributes" p) $
                        updateConfig LD.configSetOmitAnonymousContexts (getField @"omitAnonymousContexts" p) $
                            updateConfig LD.configSetFlushIntervalSeconds (getField @"flushIntervalMs" p) c
