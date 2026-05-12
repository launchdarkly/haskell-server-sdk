-- |
-- Internal helpers for assembling an 'HttpConfiguration' from a 'Config'.
--
-- This module exists separately from "LaunchDarkly.Server.Config.HttpConfiguration" so that the
-- builder can import 'Config.Internal' without creating an import cycle. ('Config.ClientContext'
-- depends on 'Config.HttpConfiguration', and 'Config.Internal' transitively depends on
-- 'Config.ClientContext' via 'DataSource.Internal', so 'Config.HttpConfiguration' itself cannot
-- import 'Config.Internal'.)
module LaunchDarkly.Server.Config.HttpConfigurationInternal
    ( makeHttpConfiguration
    ) where

import Data.ByteString (ByteString)
import Data.Generics.Product (getField)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (newManager)
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (HeaderName)

import LaunchDarkly.Server.Config.HttpConfiguration (HttpConfiguration (..), makeInstanceIdHeader)
import LaunchDarkly.Server.Config.Internal (ApplicationInfo, Config, getApplicationInfoHeader)

-- |
-- Build an 'HttpConfiguration' for the given 'Config'. A fresh TLS manager is created and a new
-- per-instance GUID v4 is generated for the 'X-LaunchDarkly-Instance-Id' header. Because the
-- returned 'defaultRequestHeaders' is shared by the polling, streaming, and event clients, every
-- outbound request carries the same stable per-instance identifier without per-channel plumbing.
--
-- The SDK key, version banner, and (optional) application tags header are also attached here.
--
-- 'clientVersion' is supplied by the caller (rather than imported from
-- "LaunchDarkly.Server.Client.Internal") to avoid an import cycle.
makeHttpConfiguration :: Text -> Config -> IO HttpConfiguration
makeHttpConfiguration clientVersion config = do
    tlsManager <- newManager tlsManagerSettings
    -- Per SCMP-server-connection-minutes-polling, every polling request must carry a per-instance
    -- GUID v4. We attach it to the default headers (rather than only on the poller) so that it is
    -- also present on streaming and event requests; this matches the cross-SDK contract tests and
    -- keeps the GUID stable for the lifetime of the SDK instance, since defaultRequestHeaders is
    -- built once and never modified after construction.
    instanceIdHdr <- makeInstanceIdHeader
    let baseHeaders =
            [ ("Authorization", encodeUtf8 $ getField @"key" config)
            , ("User-Agent", "HaskellServerClient/" <> encodeUtf8 clientVersion)
            , instanceIdHdr
            ]
        defaultRequestHeaders = addTagsHeader baseHeaders (getField @"applicationInfo" config)
        defaultRequestTimeout = Http.responseTimeoutMicro $ fromIntegral $ getField @"requestTimeoutSeconds" config * 1000000
    pure $ HttpConfiguration {..}
  where
    addTagsHeader :: [(HeaderName, ByteString)] -> Maybe ApplicationInfo -> [(HeaderName, ByteString)]
    addTagsHeader headers Nothing = headers
    addTagsHeader headers (Just info) = case getApplicationInfoHeader info of
        Nothing -> headers
        Just header -> ("X-LaunchDarkly-Tags", encodeUtf8 header) : headers
