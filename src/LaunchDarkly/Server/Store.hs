-- | This module contains details for external store implementations.
module LaunchDarkly.Server.Store
    ( StoreResult
    , FeatureKey
    , FeatureNamespace
    , PersistentDataStore (..)
    , SerializedItemDescriptor (..)
    , serializeWithPlaceholder
    , byteStringToVersionedData
    ) where

import LaunchDarkly.Server.Store.Internal
