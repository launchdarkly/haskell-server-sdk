# Change log

All notable changes to the LaunchDarkly Haskell Server-side SDK will be documented in this file. This project adheres to [Semantic Versioning](http://semver.org).

## [4.4.1](https://github.com/launchdarkly/haskell-server-sdk/compare/4.4.0...4.4.1) (2024-12-24)


### Bug Fixes

* Fix SSE newline handling ([365340a](https://github.com/launchdarkly/haskell-server-sdk/commit/365340a978a12d2da97e9d7f7d421ae63ad954ea))
* Handle optional properties in data payload ([365340a](https://github.com/launchdarkly/haskell-server-sdk/commit/365340a978a12d2da97e9d7f7d421ae63ad954ea))

## [4.4.0](https://github.com/launchdarkly/haskell-server-sdk/compare/4.3.0...4.4.0) (2024-12-05)

This release introduces the ability to enable compression of event payloads. When enabled, the SDK will compress events before sending them to the LaunchDarkly servers. This can reduce the bandwidth required to send events, which can be useful in high-traffic environments to reduce egress traffic costs.

> [!IMPORTANT]
> Relay Proxy users **MUST** upgrade to version 8.9 or higher prior to enabling this option to prevent loss of event data.
>
> However, enabling this feature is **NOT** required when using the Relay Proxy as it will manage compression automatically.


### Features

* Add option to enable compression of event payloads ([#91](https://github.com/launchdarkly/haskell-server-sdk/issues/91)) ([cb21081](https://github.com/launchdarkly/haskell-server-sdk/commit/cb2108103c5210d08b7976e9ad4d3357c332b783))

## [4.3.0](https://github.com/launchdarkly/haskell-server-sdk/compare/4.2.0...4.3.0) (2024-10-24)


### Features

* Add support for client-side prerequisite events ([#89](https://github.com/launchdarkly/haskell-server-sdk/issues/89)) ([a3bf10a](https://github.com/launchdarkly/haskell-server-sdk/commit/a3bf10acc4bebedadea9fdfcbded560cbda89d04))

## [4.2.0](https://github.com/launchdarkly/haskell-server-sdk/compare/4.1.0...4.2.0) (2024-08-23)


### Features

* Add option to omit anonymous users from index and identify events ([#87](https://github.com/launchdarkly/haskell-server-sdk/issues/87)) ([85e512a](https://github.com/launchdarkly/haskell-server-sdk/commit/85e512a80dff0e8814afb0f6e6ca334c30afe9cd))

## [4.1.0](https://github.com/launchdarkly/haskell-server-sdk/compare/4.0.4...4.1.0) (2024-03-18)


### Features

* Inline contexts for all evaluation events ([#67](https://github.com/launchdarkly/haskell-server-sdk/issues/67)) ([654df01](https://github.com/launchdarkly/haskell-server-sdk/commit/654df01c2136c0ce75f58600836b76c0e87337dd))
* Redact anonymous attributes within feature events ([#68](https://github.com/launchdarkly/haskell-server-sdk/issues/68)) ([65a3f3d](https://github.com/launchdarkly/haskell-server-sdk/commit/65a3f3d342a51941358fe0b92c3d842cb9eb8007))

## [4.0.4](https://github.com/launchdarkly/haskell-server-sdk/compare/4.0.3...4.0.4) (2024-03-05)


### Bug Fixes

* Downgrade verbose data source log messages to debug ([#76](https://github.com/launchdarkly/haskell-server-sdk/issues/76)) ([4de2b04](https://github.com/launchdarkly/haskell-server-sdk/commit/4de2b04ec99aa7a7338ac4673ec139767e410bc2))

## [4.0.3](https://github.com/launchdarkly/haskell-server-sdk/compare/4.0.2...4.0.3) (2024-02-07)


### Bug Fixes

* Bump aeson bounds to accept aeson-2.2.1.0 ([#74](https://github.com/launchdarkly/haskell-server-sdk/issues/74)) ([faee60e](https://github.com/launchdarkly/haskell-server-sdk/commit/faee60ed4fa118cefaefbbb754835c34af14fa2b))

## [4.0.2](https://github.com/launchdarkly/haskell-server-sdk/compare/4.0.1...4.0.2) (2024-02-01)


### Bug Fixes

* **deps:** Drop explicit dependency on vector ([#69](https://github.com/launchdarkly/haskell-server-sdk/issues/69)) ([3bb826d](https://github.com/launchdarkly/haskell-server-sdk/commit/3bb826da77f34983abebe84612dfa9d7f8c346c9))

## [4.0.1] - 2023-10-26
### Removed:
- Eliminated unnecessary and noisy log message

## [4.0.0] - 2023-02-21
The latest version of this SDK supports LaunchDarkly's new custom contexts feature. Contexts are an evolution of a previously-existing concept, "users." Contexts let you create targeting rules for feature flags based on a variety of different information, including attributes pertaining to users, organizations, devices, and more. You can even combine contexts to create "multi-contexts."

For detailed information about this version, please refer to the list below. For information on how to upgrade from the previous version, please read the [migration guide](https://docs.launchdarkly.com/sdk/server-side/haskell/migration-3-to-4).

### Added:
- The type `Context` from the `LaunchDarkly.Server.Context` module defines the new context model.
- All SDK methods that took a hash representing the user now accept an `LDContext`.
- Added support for [Secure Mode](https://docs.launchdarkly.com/sdk/features/secure-mode#haskell).

### Changed _(breaking changes from 3.x)_:
- The `secondary` attribute which existed in the user hash is no longer a supported feature. If you set an attribute with that name in `Context`, it will simply be a custom attribute like any other.
- Analytics event data now uses a new JSON schema due to differences between the context model and the old user model.

### Changed (requirements/dependencies/build):
- The minimum supported Stackage resolver is LTS 16.31.

### Changed (behavioral changes):
- The default polling URL has changed from `https://app.launchdarkly.com` to `https://sdk.launchdarkly.com`.
- Several optimizations within the flag evaluation logic have improved the performance of evaluations. For instance, target lists are now stored internally as sets for faster matching.

### Removed:
- Removed the `User` type and all associated functions from the `LaunchDarkly.Server.User` module.
- Removed support for the `secondary` meta-attribute in the user hash.
- The `alias` method no longer exists because alias events are not needed in the new context model.
- The `configSetInlineUsersInEvents` configuration option no longer exists because it is not relevant in the new context model.
- Removed all types and options that were deprecated as of the most recent 3.x release.
- The old Redis store integration has been removed from this repository and published to its own separate package. You can learn more by reviewing the [Haskell redis docs](https://docs.launchdarkly.com/sdk/features/storing-data/redis#haskell) or reviewing the published package on [Hackage](https://hackage.haskell.org/package/launchdarkly-server-sdk-redis-hedis).

### Deprecated:

The following methods in `TestData` have been deprecated and replaced with new context-aware options.
- `variationForAllUsers` was replaced with `variationForAll`
- `valueForAllUsers` was replaced with `valueForAll`
- `variationForUser` was replaced with `variationForKey`
- `ifMatch` was replaced with `ifMatchContext`
- `ifNotMatch` was replaced with `ifNotMatchContext`
- `andMatch` was replaced with `andMatchContext`
- `andNotMatch` was replaced with `andNotMatchContext`

The config method `configSetUserKeyLRUCapacity` has been deprecated and replaced with `configSetContextKeyLRUCapacity`.

## [3.1.1] - 2023-02-17
### Fixed:
- The polling thread will be shutdown if the LaunchDarkly polling API returns an unrecoverable response code.
- Updated dependency versions in the Redis store package to mirror the SDK requirements.

## [3.1.0] - 2023-01-27
### Added:
- New `ApplicationInfo` type, for configuration of application metadata that may be used in LaunchDarkly analytics or other product features. This does not affect feature flag evaluations.

## [3.0.4] - 2023-01-09
### Changed:
- Expanded upper version to allow `aeson-2.1`.  (Thanks, [vrom911](https://github.com/launchdarkly/haskell-server-sdk/pull/49))
- Expanded upper version to allow `hashtables-1.3`. (Thanks, [vrom911](https://github.com/launchdarkly/haskell-server-sdk/pull/49))
- Expanded upper version to allow `mtl-2.3`. (Thanks, [vrom911](https://github.com/launchdarkly/haskell-server-sdk/pull/49))
- Expanded upper version to allow `text-2.0`. (Thanks, [vrom911](https://github.com/launchdarkly/haskell-server-sdk/pull/49))
- Expanded upper version to allow `time-1.12`. (Thanks, [vrom911](https://github.com/launchdarkly/haskell-server-sdk/pull/49))

## [3.0.3] - 2022-11-08
### Added:
- Commit generated Cabal file per the [stack recommendations](https://github.com/commercialhaskell/stack/issues/5210) (Thanks, [philderbeast](https://github.com/launchdarkly/haskell-server-sdk/pull/44))

### Changed:
- Regenerated `.hlint.yaml` with defaults and counts (Thanks, [philderbeast](https://github.com/launchdarkly/haskell-server-sdk/pull/45)!)

### Fixed:
- Fixed hlint "pattern parser error" lint. (Thanks, [philderbeast](https://github.com/launchdarkly/haskell-server-sdk/pull/46)!)

## [3.0.2] - 2022-09-20
### Changed:
- Expanded upper version to allow lens 5.1. (Thanks, [bmillwood](https://github.com/launchdarkly/haskell-server-sdk/pull/42)!)

## [3.0.1] - 2022-07-01
### Fixed:
- Fixed Aeson 2.0 compatibility layer.

## [3.0.0] - 2022-06-27
### Added:
- Add flag support for the client side availability property, as well
as the older the ability to decode from the older clientSide format.
- The new allFlagsState function should be used instead of allFlags if you are passing flag data to the front end for use with the JavaScript SDK. It preserves some flag metadata that the front end requires in order to send analytics events correctly. Versions 2.5.0 and above of the JavaScript SDK are able to use this metadata, but the output of allFlagsState will still work with older versions.
- It is now possible to inject feature flags into the client from local JSON files, replacing the normal LaunchDarkly connection. This would typically be for testing purposes. See LaunchDarkly.Server.Integrations.FileData.
- LaunchDarkly.Server.Integrations.TestData is another new way to inject feature flag data programmatically into the SDK for testing—either with fixed values for each flag, or with targets and/or rules that can return different values for different users. Unlike FileData, this mechanism does not use any external resources, only the data that your test code has provided.

### Changed:
- CI builds now include a cross-platform test suite implemented in https://github.com/launchdarkly/sdk-test-harness. This covers many test cases that are also implemented in unit tests, but may be extended in the future to ensure consistent behavior across SDKs in other areas.
- The SDK will track the last known server time as specified in the Date header when sending events. This value, along with the current system time, will be used to determine if debug event should still be sent.
- VariationIndex has been changed from Natural to Integer.

### Fixed:
- When evaluating against a user attribute, if the attribute is null, it should always be treated as a non-match.

## [2.2.0] - 2021-06-17
### Added:
- The SDK now supports the ability to control the proportion of traffic allocation to an experiment. This works in conjunction with a new platform feature now available to early access customers.

## [2.1.1] - 2021-03-05
### Changed:
- Updated dependency ranges. Thanks @dbaynard !

## [2.1.0] - 2021-02-04
### Added:
- Added the `alias` function. This can be used to associate two user objects for analytics purposes by generating an alias event.

## [2.0.2] - 2020-10-13
### Fixed:
- Removed unused legacy &#34;sel&#34; field from flag model.

## [2.0.1] - 2020-08-28
### Fixed:
- Expanded dependency supported version range. Thanks @dbaynard!


## [2.0.0] - 2020-04-21
### Changed:
- The function `userSetKey` is now takes `Text` instead of `Maybe Text` for the key. This originally existed for compatibility with an internal test suite.

### Removed:
- The constructor `EvalErrorUserNotSpecified` has been removed from `EvalErrorKind`.

## [1.0.4] - 2020-04-06
### Fixed:
- Widened time and lens dependency ranges. Thanks @nbouscal!

## [1.0.3] - 2020-03-31
### Fixed:
- Standardize streaming retry behavior. Changed handling of status codes. Exponential back-off is now reset after 60 seconds of successful streaming.

## [1.0.2] - 2020-03-16
### Fixed:
- Added a timeout on SSE reads to 5 minutes.
- Fixed an issue where reading SSE streams would burn CPU in certain edge cases

## [1.0.1] - 2020-03-02
### Fixed:
- Client initialization status is now correctly determined by checking the feature store instead of an always in memory value. This is particularly important for usage of daemon mode. In the current implementation daemon mode evaluation always returns the default fallback value because the client never becomes initialized.

## [1.0.0] - 2020-02-24
### Fixed:
- Several haddock typos.
- Added `configSetStoreTTL`, and `configSetUseLdd` to `LaunchDarkly.Server` export list.
- Added `package.yaml` version constraints.

## [0.2.1] - 2020-02-21
### Fixed:
- Added an event delivery retry mechanism. The SDK will now try to deliver events again after one second before dropping them.
- Added a payload identity header for event delivery to prevent duplication in certain edge cases.
- Made many fields strict for more deterministic memory usage.

## [0.2.0] - 2020-02-10
### Added:
- Added support for utilizing external features stores. See `LaunchDarkly.Server.Store` for details on implementing a store. You can configure usage of a specific store with `configSetStoreBackend`.
- Added support for Redis as an external feature store. See the `launchdarkly-server-sdk-redis` package for details.
- Added support for LaunchDarkly daemon mode configurable with `configSetUseLdd`. To learn more, read [Using daemon mode](https://docs.launchdarkly.com/home/relay-proxy/using#using-daemon-mode).
### Fixed:
- Incorrect ToJSON instances for flag rules and operators.
- Updated bucketing logic to fallback to last variation instead of producing an error.
- Refactored streaming implementation.
- Stopped sending empty event payloads.


## [0.1.1] - 2019-12-10
### Fixed
- Corrected internal HTTP client user agent format.
