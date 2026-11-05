Contributing to the LaunchDarkly Server-side SDK for Haskell
================================================

LaunchDarkly has published an [SDK contributor's guide](https://docs.launchdarkly.com/sdk/concepts/contributors-guide) that provides a detailed explanation of how our SDKs work. See below for additional information on how to contribute to this SDK.

Submitting bug reports and feature requests
------------------

The LaunchDarkly SDK team monitors the [issue tracker](https://github.com/launchdarkly/haskell-server-sdk/issues) in the SDK repository. Bug reports and feature requests specific to this SDK should be filed in this issue tracker. The SDK team will respond to all newly filed issues within two business days.

Submitting pull requests
------------------

We encourage pull requests and other contributions from the community. Before submitting pull requests, ensure that all temporary or unintended code is removed. Don't worry about adding reviewers to the pull request; the LaunchDarkly SDK team will add themselves. The SDK team will acknowledge all pull requests within two business days.

Build instructions
------------------

### Prerequisites

The SDK is built using [Stack](https://docs.haskellstack.org/en/stable/README/). If you use [Homebrew](https://brew.sh/), you can install the `haskell-stack` package to install Stack.

The SDK does not need any system libraries. The [`regex-pcre-builtin`](https://hackage.haskell.org/package/regex-pcre-builtin) package that the SDK uses for regular expressions includes the PCRE C sources and builds them with the package.

### Building

To build the SDK without running any tests:
```
stack build
```

### Testing

To build the SDK and run all unit tests:
```
stack test
```
