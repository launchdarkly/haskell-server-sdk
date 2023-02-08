-- | This module re-exports the User, Client, Config, and Context modules.
module LaunchDarkly.Server
    ( -- * Config
      Config
    , makeConfig
    , configSetKey
    , configSetBaseURI
    , configSetStreamURI
    , configSetEventsURI
    , configSetStreaming
    , configSetInitialRetryDelay
    , configSetAllAttributesPrivate
    , configSetPrivateAttributeNames
    , configSetFlushIntervalSeconds
    , configSetPollIntervalSeconds
    , configSetContextKeyLRUCapacity
    , configSetUserKeyLRUCapacity
    , configSetEventsCapacity
    , configSetLogger
    , configSetManager
    , configSetSendEvents
    , configSetOffline
    , configSetRequestTimeoutSeconds
    , configSetStoreBackend
    , configSetStoreTTL
    , configSetUseLdd
    , configSetDataSourceFactory
    , configSetApplicationInfo
    , ApplicationInfo
    , makeApplicationInfo
    , withApplicationValue

      -- * User
    , User
    , makeUser
    , userSetKey
    , userSetSecondary
    , userSetIP
    , userSetCountry
    , userSetEmail
    , userSetFirstName
    , userSetLastName
    , userSetAvatar
    , userSetName
    , userSetAnonymous
    , userSetCustom
    , userSetPrivateAttributeNames

      -- * Client
    , Client
    , makeClient
    , clientVersion
    , boolVariation
    , boolVariationDetail
    , stringVariation
    , stringVariationDetail
    , intVariation
    , intVariationDetail
    , doubleVariation
    , doubleVariationDetail
    , jsonVariation
    , jsonVariationDetail
    , EvaluationDetail (..)
    , EvaluationReason (..)
    , EvalErrorKind (..)
    , allFlagsState
    , AllFlagsState
    , secureModeHash
    , close
    , flushEvents
    , identify
    , track
    , Status (..)
    , getStatus

      -- * Context
    , Context
    , makeContext
    , makeMultiContext
    , withName
    , withAnonymous
    , withAttribute
    , withPrivateAttributes
    , isValid
    , getError
    , getIndividualContext
    , getValue
    , getValueForReference
    ) where

import LaunchDarkly.Server.Client
import LaunchDarkly.Server.Config
import LaunchDarkly.Server.Context
import LaunchDarkly.Server.User
