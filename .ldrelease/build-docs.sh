#!/bin/bash

set -e

# This only runs in the Linux build, since the docs are the same for all platforms.
stack --no-terminal haddock

cp -a "$(stack --no-terminal path --dist-dir)"/doc/html/launchdarkly-server-sdk/* "${LD_RELEASE_DOCS_DIR}"
