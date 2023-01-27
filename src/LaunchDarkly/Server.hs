-- | This module re-exports the User, Client, and Config modules.

module LaunchDarkly.Server
    ( Config
    , makeConfig
    , configSetKey
    , configSetBaseURI
    , configSetStreamURI
    , configSetEventsURI
    , configSetStreaming
    , configSetAllAttributesPrivate
    , configSetPrivateAttributeNames
    , configSetFlushIntervalSeconds
    , configSetPollIntervalSeconds
    , configSetUserKeyLRUCapacity
    , configSetInlineUsersInEvents
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
    , EvaluationDetail(..)
    , EvaluationReason(..)
    , EvalErrorKind(..)
    , allFlagsState
    , AllFlagsState
    , close
    , flushEvents
    , identify
    , track
    , Status(..)
    , getStatus
    -- Context related functions and types
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

import LaunchDarkly.Server.User
import LaunchDarkly.Server.Config
import LaunchDarkly.Server.Client
import LaunchDarkly.Server.Context
