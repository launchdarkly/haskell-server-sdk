#!/bin/bash

set -e

sed -i "s/^\(version:[^0-9]\+\).*/\1${LD_RELEASE_VERSION}/" package.yaml
sed -i "s/^\(version:[^0-9]\+\).*/\1${LD_RELEASE_VERSION}/" launchdarkly-server-sdk.cabal
sed -i "s/clientVersion = \".*\"/clientVersion = \"${LD_RELEASE_VERSION}\"/" src/LaunchDarkly/Server/Client/Internal.hs
