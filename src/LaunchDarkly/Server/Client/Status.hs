module LaunchDarkly.Server.Client.Status
    ( Status(..)
    , transitionStatus
    )
    where

-- | The status of the client initialization.
data Status
    = Uninitialized
      -- ^ The client has not yet finished connecting to LaunchDarkly.
    | Unauthorized
      -- ^ The client attempted to connect to LaunchDarkly and was denied.
    | Initialized
      -- ^ The client has successfuly connected to LaunchDarkly.
    | ShuttingDown
      -- ^ The client is being terminated
    deriving (Show, Eq)

transitionStatus :: Status -> Status -> Status
transitionStatus requestedStatus oldStatus = 
    case requestedStatus of
        -- Only allow setting Initialized if Uninitialized
        Initialized   -> if oldStatus == Uninitialized then Initialized  else oldStatus
        -- Only allow setting status if not ShuttingDown
        _             -> if oldStatus == ShuttingDown  then ShuttingDown else requestedStatus
