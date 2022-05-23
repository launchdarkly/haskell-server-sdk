module Utils where

import Control.Concurrent (threadDelay)
import Data.Text (Text)
import Data.Aeson (Value(..))
import qualified LaunchDarkly.Server as LD
import Types
import Data.Generics.Product (getField, setField)

createClient :: CreateClientParams -> IO LD.Client
createClient p = LD.makeClient $ createConfig $ getField @"configuration" p

waitClient :: LD.Client -> IO ()
waitClient client = do
  status <- LD.getStatus client
  case status of
    LD.Initialized -> return ()
    _ -> threadDelay (1 * 1000) >> waitClient client

createConfig :: ConfigurationParams -> LD.Config
createConfig p = do
    let config = LD.makeConfig $ getField @"credential" p
        streamerConfig = streamingConfig config $ getField @"streaming" p
    eventConfig streamerConfig $ getField @"events" p

updateConfig :: (a -> LD.Config -> LD.Config) -> Maybe a -> LD.Config -> LD.Config
updateConfig f Nothing config = config
updateConfig f (Just x) config = f x config

-- TODO(mmk) We aren't handling the initialRetryDelayMs because the SDK doesn't seem to support it
streamingConfig :: LD.Config -> Maybe StreamingParams -> LD.Config
streamingConfig c Nothing = c
streamingConfig c (Just p) = updateConfig LD.configSetStreamURI (getField @"baseUri" p) c

eventConfig :: LD.Config -> Maybe EventParams -> LD.Config
eventConfig c Nothing = updateConfig LD.configSetSendEvents (Just False) c
eventConfig c (Just p) = updateConfig LD.configSetEventsURI (getField @"baseUri" p)
    $ updateConfig LD.configSetEventsCapacity (getField @"capacity" p)
    $ updateConfig LD.configSetAllAttributesPrivate (getField @"allAttributesPrivate" p)
    $ updateConfig LD.configSetPrivateAttributeNames (getField @"globalPrivateAttributes" p)
    $ updateConfig LD.configSetFlushIntervalSeconds (getField @"flushIntervalMs" p)
    $ updateConfig LD.configSetInlineUsersInEvents (getField @"inlineUsers" p) c
