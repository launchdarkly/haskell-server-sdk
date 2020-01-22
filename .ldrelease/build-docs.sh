#!/bin/bash

set -e

# This only runs in the Linux build, since the docs are the same for all platforms.

PROJECT_DIR=$(pwd)

stack --no-terminal haddock

mkdir -p $PROJECT_DIR/artifacts
cd $(stack --no-terminal path --dist-dir)/doc/html/launchdarkly-server-sdk
zip -r $PROJECT_DIR/artifacts/docs.zip *
