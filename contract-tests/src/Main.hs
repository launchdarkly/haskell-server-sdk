{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Strict #-}

import Control.Monad.Trans (liftIO)
import Control.Concurrent (MVar, newEmptyMVar, forkIO, putMVar, takeMVar)
import System.Timeout (timeout)
import Data.Aeson (ToJSON, toJSON, Value (..), encode, decode )
import Data.Generics.Product (getField)
import Data.Scientific (toRealFloat, floatingOrInteger)
import Data.IORef
import Data.Maybe
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Types
import Types
import Web.Scotty
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text.Lazy as LTB
import qualified LaunchDarkly.Server as LD
import qualified Utils
import qualified Data.Set as S
import Data.Function ((&))
import LaunchDarkly.Server (Context, withAttribute, withPrivateAttributes, getError)
import LaunchDarkly.Server.Reference (makeReference)
import Data.Text.Lazy (toStrict, fromStrict)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)

data AppState = AppState { clients :: M.Map Int LD.Client, counter :: Int }

data AppStatus = AppStatus { name :: !Text, clientVersion :: !Text, capabilities :: [Text] } deriving (Show, Generic)
instance ToJSON AppStatus

getAppStatus :: ActionM ()
getAppStatus = json AppStatus
    { name = "haskell-server-sdk"
    , clientVersion = LD.clientVersion
    , capabilities =
        [ "server-side"
        , "server-side-polling"
        , "strongly-typed"
        , "all-flags-with-reasons"
        , "all-flags-client-side-only"
        , "all-flags-details-only-for-tracked-flags"
        , "context-type"
        , "secure-mode-hash"
        , "tags"
        , "inline-context"
        , "anonymous-redaction"
        , "omit-anonymous-contexts"
        ]
    }

shutdownService :: MVar () -> ActionM ()
shutdownService shutdownMVar = liftIO $ putMVar shutdownMVar ()

createClient :: IORef AppState -> ActionM ()
createClient appStateRef = do
    createClientParams <- jsonData
    client <- liftIO $ Utils.createClient createClientParams
    initialized <- liftIO $ (isJust <$> timeout (5000 * 1000) (Utils.waitClient client))
    let configuration = getField @"configuration" createClientParams
        canFail = fromMaybe False (getField @"initCanFail" configuration)
     in case (initialized || canFail) of
      True -> do
          newCounter <- liftIO $ atomicModifyIORef' appStateRef $ \state -> do
              let c = clients state
                  count = succ $ counter state
              (state { clients = M.insert count client c, counter = count }, count)
          addHeader "Location" (LTB.pack $ "/client/" ++ (show newCounter))
      False -> status status500

runCommand :: IORef AppState -> ActionM ()
runCommand appStateRef = do
    clientId <- param "clientId"
    commandParams <- jsonData :: ActionM CommandParams
    appState <- liftIO $ readIORef appStateRef
    let client = M.lookup (read clientId :: Int) (clients appState)
    case client of
      Nothing -> error "Invalid client provided"
      Just c -> case (command commandParams) of
                  "evaluate" -> evaluateCommand c (evaluate commandParams)
                  "evaluateAll" -> evaluateAllCommand c (evaluateAll commandParams)
                  "customEvent" -> customCommand c (customEvent commandParams)
                  "identifyEvent" -> identifyCommand c (identifyEvent commandParams)
                  "flushEvents" -> liftIO $ LD.flushEvents c
                  "contextBuild" -> contextBuildCommand $ contextBuild commandParams
                  "contextConvert" -> contextConvertCommand $ contextConvert commandParams
                  "secureModeHash" -> secureModeHashCommand c (secureModeHash commandParams)
                  _ -> error "An unknown command was requested"

identifyCommand :: LD.Client -> Maybe IdentifyEventParams -> ActionM ()
identifyCommand _ Nothing = error "Missing identify event params"
identifyCommand c (Just p) = liftIO $ LD.identify c (getField @"context" p)

contextBuildCommand :: Maybe ContextBuildParams -> ActionM ()
contextBuildCommand Nothing = error "Missing context build params"
contextBuildCommand (Just ContextBuildParams { single = Just buildParam}) =
  json $ createContextResponse $ contextBuildSingle buildParam
contextBuildCommand (Just ContextBuildParams { multi = Just buildParams}) =
  json $ createContextResponse $ LD.makeMultiContext $ L.map contextBuildSingle buildParams
contextBuildCommand _ = json $ ContextResponse { output = Nothing, errorMessage = Just "No build parameters provided" }

contextBuildSingle :: ContextBuildParam -> LD.Context
contextBuildSingle (ContextBuildParam {kind, key, name, anonymous, private, custom}) =
  let context = LD.makeContext key (fromMaybe "user" kind)
        & contextWithAttribute "name" (String <$> name)
        & contextWithAttribute "anonymous" (Bool <$> anonymous)
        & withPrivateAttributes (S.map makeReference (fromMaybe S.empty private))
  in HM.foldrWithKey (\k v c -> contextWithAttribute k (Just v) c) context (fromMaybe HM.empty custom)

contextWithAttribute :: Text -> (Maybe Value) -> Context -> Context
contextWithAttribute _ Nothing c = c
contextWithAttribute attr (Just v) c = withAttribute attr v c

contextConvertCommand :: Maybe ContextConvertParams -> ActionM ()
contextConvertCommand Nothing = error "Missing context convert params"
contextConvertCommand (Just ContextConvertParams { input }) =
  let context = decode $ encodeUtf8 $ fromStrict input
  in case context of
    Just ctx -> json $ createContextResponse ctx
    Nothing -> json $ ContextResponse { output = Nothing, errorMessage = Just "Error decoding input string" }

createContextResponse :: Context -> ContextResponse
createContextResponse c = case LD.isValid c of
  True -> ContextResponse { output = Just $ (toStrict (decodeUtf8 (encode c))), errorMessage = Nothing }
  False -> ContextResponse { output = Nothing, errorMessage = Just $ getError c }

secureModeHashCommand :: LD.Client -> Maybe SecureModeHashParams -> ActionM ()
secureModeHashCommand _ Nothing = error "Missing secure mode hash params"
secureModeHashCommand _ (Just (SecureModeHashParams { context = Nothing })) = error "This SDK does not support secure mode on non-context types"
secureModeHashCommand c (Just (SecureModeHashParams { context = Just context })) = json $ SecureModeHashResponse { result = LD.secureModeHash c context }

customCommand :: LD.Client -> Maybe CustomEventParams -> ActionM ()
customCommand _ Nothing = error "Missing custom event params"
customCommand c (Just p) = liftIO $ LD.track c (getField @"context" p) (getField @"eventKey" p) (getField @"dataValue" p) (getField @"metricValue" p)

evaluateCommand :: LD.Client -> Maybe EvaluateFlagParams -> ActionM ()
evaluateCommand _ Nothing = error "Missing evaluate params"
evaluateCommand c (Just p)
    | (detail p) == True = do
        d <- liftIO $ evaluateWithDetail c context flagKey valueType defaultValue
        json d
    | otherwise = do
        d <- liftIO $ evaluateWithoutDetail c context flagKey valueType defaultValue
        json d
        where context = (getField @"context" p)
              flagKey = (getField @"flagKey" p)
              valueType = (getField @"valueType" p)
              defaultValue = (getField @"defaultValue" p)

toFlagResponseWithDetail :: ToJSON a => LD.EvaluationDetail a -> EvaluateFlagResponse
toFlagResponseWithDetail d = EvaluateFlagResponse
    { value = toJSON $ (getField @"value" d)
    , variationIndex = (getField @"variationIndex" d)
    , reason = Just $ (getField @"reason" d)
    }

toFlagResponseWithoutDetail :: ToJSON a => a -> EvaluateFlagResponse
toFlagResponseWithoutDetail v = EvaluateFlagResponse
    { value = toJSON $ v
    , variationIndex = Nothing
    , reason = Nothing
    }

evaluateWithDetail :: LD.Client -> LD.Context -> Text -> Text -> Value -> IO EvaluateFlagResponse
evaluateWithDetail c context flagKey "bool" (Bool v) = toFlagResponseWithDetail <$> LD.boolVariationDetail c flagKey context v
evaluateWithDetail c context flagKey "int" (Number v) = case floatingOrInteger v of
      Left _ -> error("Invalid int format")
      Right x -> toFlagResponseWithDetail <$> LD.intVariationDetail c flagKey context x
evaluateWithDetail c context flagKey "double" (Number v) = toFlagResponseWithDetail <$> LD.doubleVariationDetail c flagKey context (toRealFloat v)
evaluateWithDetail c context flagKey "string" (String v) = toFlagResponseWithDetail <$> LD.stringVariationDetail c flagKey context v
evaluateWithDetail c context flagKey "any" v = toFlagResponseWithDetail <$> LD.jsonVariationDetail c flagKey context v
evaluateWithDetail _ _ _ _ _ = error("Invalid type provided")

evaluateWithoutDetail :: LD.Client -> LD.Context -> Text -> Text -> Value -> IO EvaluateFlagResponse
evaluateWithoutDetail c context flagKey "bool" (Bool v) = toFlagResponseWithoutDetail <$> LD.boolVariation c flagKey context v
evaluateWithoutDetail c context flagKey "int" (Number v) = case floatingOrInteger v of
        Left _ -> error("Invalid int format")
        Right x -> toFlagResponseWithoutDetail <$> LD.intVariation c flagKey context x
evaluateWithoutDetail c context flagKey "double" (Number v) = toFlagResponseWithoutDetail <$> LD.doubleVariation c flagKey context (toRealFloat v)
evaluateWithoutDetail c context flagKey "string" (String v) = toFlagResponseWithoutDetail <$> LD.stringVariation c flagKey context v
evaluateWithoutDetail c context flagKey "any" v = toFlagResponseWithoutDetail <$> LD.jsonVariation c flagKey context v
evaluateWithoutDetail _ _ _ _ _ = error("Invalid type provided")

evaluateAllCommand :: LD.Client -> Maybe EvaluateAllFlagsParams -> ActionM ()
evaluateAllCommand _ Nothing = error "Missing evaluate all params"
evaluateAllCommand c (Just p) = do
    s <- liftIO $ LD.allFlagsState c (getField @"context" p) (getField @"clientSideOnly" p) (getField @"withReasons" p) (getField @"detailsOnlyForTrackedFlags" p)
    json $ EvaluateAllFlagsResponse { state = s }

stopClient :: IORef AppState -> ActionM ()
stopClient appStateRef = do
    clientIdParam <- param "clientId"
    appState <- liftIO $ readIORef appStateRef
    let clientId = read clientIdParam :: Int
        client = M.lookup clientId (clients appState)
    case client of
      Nothing -> error "Invalid client provided"
      Just c ->  liftIO $ do
          LD.close c
          modifyIORef' appStateRef (\a -> a { clients = M.delete clientId (clients a)})

routes :: IORef AppState -> MVar () -> ScottyM ()
routes appStateRef shutdownMVar = do
    get "/" getAppStatus
    delete "/" (shutdownService shutdownMVar)
    post "/" (createClient appStateRef)
    post "/client/:clientId" (runCommand appStateRef)
    delete "/client/:clientId" (stopClient appStateRef)

server :: IO ()
server = do
    appStateRef <- newIORef $ AppState { clients = M.empty, counter = 0 }
    shutdownMVar <- newEmptyMVar
    _ <- forkIO $ scotty 8000 (routes appStateRef shutdownMVar)
    takeMVar shutdownMVar

main :: IO ()
main = server
