# Change log

All notable changes to the LaunchDarkly Haskell Server-side SDK will be documented in this file. This project adheres to [Semantic Versioning](http://semver.org).

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
