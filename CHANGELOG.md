# Change log

All notable changes to the LaunchDarkly Haskell Server-side SDK will be documented in this file. This project adheres to [Semantic Versioning](http://semver.org).

## [0.2.0] - 2020-02-10
### Added:
- Added support for utilizing external features stores. See `LaunchDarkly.Server.Store` for details on implementing a store. You can configure usage of a specific store with `configSetStoreBackend`.
- Added support for Redis as an external feature store. See the `launchdarkly-server-sdk-redis` package for details.
- Added support for LaunchDarkly daemon mode configurable with `configSetUseLdd`.
### Fixed:
- Incorrect ToJSON instances for flag rules and operators.
- Updated bucketing logic to fallback to last variation instead of producing an error.
- Refactored streaming implementation.
- Stopped sending empty event payloads.


## [0.1.1] - 2019-12-10
### Fixed
- Corrected internal HTTP client user agent format.
