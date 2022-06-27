module LaunchDarkly.Server.Config.ClientContext
    (ClientContext(..))
    where

import Control.Monad.Logger (LoggingT)

import LaunchDarkly.Server.Config.HttpConfiguration (HttpConfiguration)

data ClientContext = ClientContext 
    { runLogger :: !(LoggingT IO () -> IO ())
    , httpConfiguration :: !HttpConfiguration
    }

