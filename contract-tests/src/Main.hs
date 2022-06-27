import Control.Monad.Trans (liftIO, lift)
import Control.Concurrent (MVar, newEmptyMVar, forkIO, putMVar, takeMVar)
import System.Timeout (timeout)
import Data.Aeson (FromJSON, ToJSON, toJSON, Value (..) )
import Data.Generics.Product (getField, setField)
import Data.Scientific (toRealFloat, fromFloatDigits, floatingOrInteger)
import Data.IORef
import Data.Maybe
import Data.Text
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Types
import Types
import Web.Scotty
import qualified Data.List
import qualified Data.Map as M
import qualified Data.Text.Lazy as LTB
import qualified LaunchDarkly.Server as LD
import qualified Utils

data AppState = AppState { clients :: M.Map Int LD.Client, counter :: Int }

data AppStatus = AppStatus { name :: !Text, clientVersion :: !Text, capabilities :: [Text] } deriving (Show, Generic)
instance ToJSON AppStatus

getAppStatus :: ActionM ()
getAppStatus = json AppStatus
    { name = "haskell-server-sdk"
    , clientVersion = LD.clientVersion
    , capabilities = ["server-side", "strongly-typed", "all-flags-with-reasons", "all-flags-client-side-only", "all-flags-details-only-for-tracked-flags"]
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
                  "aliasEvent" -> aliasCommand c (aliasEvent commandParams)
                  "identifyEvent" -> identifyCommand c (identifyEvent commandParams)
                  "flushEvents" -> liftIO $ LD.flushEvents c

aliasCommand :: LD.Client -> Maybe AliasEventParams -> ActionM ()
aliasCommand _ Nothing = error "Missing alias event params"
aliasCommand c (Just p) = liftIO $ LD.alias c (getField @"user" p) (getField @"previousUser" p)

identifyCommand :: LD.Client -> Maybe IdentifyEventParams -> ActionM ()
identifyCommand _ Nothing = error "Missing identify event params"
identifyCommand c (Just p) = liftIO $ LD.identify c (getField @"user" p)

customCommand :: LD.Client -> Maybe CustomEventParams -> ActionM ()
customCommand _ Nothing = error "Missing custom event params"
customCommand c (Just p) = liftIO $ LD.track c (getField @"user" p) (getField @"eventKey" p) (getField @"dataValue" p) (getField @"metricValue" p)

evaluateCommand :: LD.Client -> Maybe EvaluateFlagParams -> ActionM ()
evaluateCommand _ Nothing = error "Missing evaluate params"
evaluateCommand c (Just p)
    | (detail p) == True = do
        d <- liftIO $ evaluateWithDetail c user flagKey valueType defaultValue
        json d
    | otherwise = do
        d <- liftIO $ evaluateWithoutDetail c user flagKey valueType defaultValue
        json d
        where user = (getField @"user" p)
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

evaluateWithDetail :: LD.Client -> LD.User -> Text -> Text -> Value -> IO EvaluateFlagResponse
evaluateWithDetail c user flagKey "bool" (Bool v) = toFlagResponseWithDetail <$> LD.boolVariationDetail c flagKey user v
evaluateWithDetail c user flagKey "int" (Number v) = case floatingOrInteger v of
      Left _ -> error("Invalid int format")
      Right x -> toFlagResponseWithDetail <$> LD.intVariationDetail c flagKey user x
evaluateWithDetail c user flagKey "double" (Number v) = toFlagResponseWithDetail <$> LD.doubleVariationDetail c flagKey user (toRealFloat v)
evaluateWithDetail c user flagKey "string" (String v) = toFlagResponseWithDetail <$> LD.stringVariationDetail c flagKey user v
evaluateWithDetail c user flagKey "any" v = toFlagResponseWithDetail <$> LD.jsonVariationDetail c flagKey user v

evaluateWithoutDetail :: LD.Client -> LD.User -> Text -> Text -> Value -> IO EvaluateFlagResponse
evaluateWithoutDetail c user flagKey "bool" (Bool v) = toFlagResponseWithoutDetail <$> LD.boolVariation c flagKey user v
evaluateWithoutDetail c user flagKey "int" (Number v) = case floatingOrInteger v of
        Left _ -> error("Invalid int format")
        Right x -> toFlagResponseWithoutDetail <$> LD.intVariation c flagKey user x
evaluateWithoutDetail c user flagKey "double" (Number v) = toFlagResponseWithoutDetail <$> LD.doubleVariation c flagKey user (toRealFloat v)
evaluateWithoutDetail c user flagKey "string" (String v) = toFlagResponseWithoutDetail <$> LD.stringVariation c flagKey user v
evaluateWithoutDetail c user flagKey "any" v = toFlagResponseWithoutDetail <$> LD.jsonVariation c flagKey user v

evaluateAllCommand :: LD.Client -> Maybe EvaluateAllFlagsParams -> ActionM ()
evaluateAllCommand _ Nothing = error "Missing evaluate all params"
evaluateAllCommand c (Just p) = do
    s <- liftIO $ LD.allFlagsState c (getField @"user" p) (getField @"clientSideOnly" p) (getField @"withReasons" p) (getField @"detailsOnlyForTrackedFlags" p)
    json $ EvaluateAllFlagsResponse { state = s }

stopClient :: IORef AppState -> ActionM ()
stopClient appStateRef = do
    clientId <- param "clientId"
    appState <- liftIO $ readIORef appStateRef
    let id = read clientId :: Int
        client = M.lookup id (clients appState)
    case client of
      Nothing -> error "Invalid client provided"
      Just c ->  liftIO $ do
          LD.close c
          modifyIORef' appStateRef (\a -> a { clients = M.delete id (clients a)})

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
    forkIO $ scotty 8000 (routes appStateRef shutdownMVar)
    takeMVar shutdownMVar

main = server
