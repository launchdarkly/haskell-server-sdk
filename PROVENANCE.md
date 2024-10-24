## Verifying SDK build provenance with the SLSA framework

LaunchDarkly uses the [SLSA framework](https://slsa.dev/spec/v1.0/about) (Supply-chain Levels for Software Artifacts) to help developers make their supply chain more secure by ensuring the authenticity and build integrity of our published SDK packages.

As part of [SLSA requirements for level 3 compliance](https://slsa.dev/spec/v1.0/requirements), LaunchDarkly publishes provenance about our SDK package builds using [GitHub's generic SLSA3 provenance generator](https://github.com/slsa-framework/slsa-github-generator/blob/main/internal/builders/generic/README.md#generation-of-slsa3-provenance-for-arbitrary-projects) for distribution alongside our packages. These attestations are available for download from the GitHub release page for the release version under Assets > `multiple.intoto.jsonl`.

To verify SLSA provenance attestations, we recommend using [slsa-verifier](https://github.com/slsa-framework/slsa-verifier). Example usage for verifying SDK packages is included below:

<!-- x-release-please-start-version -->
```
# Set the version of the SDK to verify
SDK_VERSION=4.3.0
```
<!-- x-release-please-end -->


```
# Download package from Hackage
$ curl -O https://hackage.haskell.org/package/launchdarkly-server-sdk-${SDK_VERSION}/launchdarkly-server-sdk-${SDK_VERSION}.tar.gz

# Download provenance from Github release into same directory
$ curl --location -O \
  https://github.com/launchdarkly/haskell-server-sdk/releases/download/${SDK_VERSION}/launchdarkly-server-sdk-${SDK_VERSION}.tar.gz.intoto.jsonl

# Run slsa-verifier to verify provenance against package artifacts
$ slsa-verifier verify-artifact \
--provenance-path launchdarkly-server-sdk-${SDK_VERSION}.tar.gz.intoto.jsonl \
--source-uri github.com/launchdarkly/haskell-server-sdk \
launchdarkly-server-sdk-${SDK_VERSION}.tar.gz
```

Below is a sample of expected output.
```
Verified signature against tlog entry index 76419919 at URL: https://rekor.sigstore.dev/api/v1/log/entries/24296fb24b8ad77a56491ff79d66537ddc16157d7ba7f31d59f0929cc6ce75ed98a0efed7fd3272a
Verified build using builder "https://github.com/slsa-framework/slsa-github-generator/.github/workflows/generator_generic_slsa3.yml@refs/tags/v1.7.0" at commit dcf5e4be8e0c176c875919dcd5877193fac4f634
Verifying artifact launchdarkly-server-sdk-4.0.4.tar.gz: PASSED

PASSED: Verified SLSA provenance
```

Alternatively, to verify the provenance manually, the SLSA framework specifies [recommendations for verifying build artifacts](https://slsa.dev/spec/v1.0/verifying-artifacts) in their documentation.

**Note:** These instructions do not apply when building our SDKs from source.
