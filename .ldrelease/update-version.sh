#!/bin/bash

set -e

TARGET_FILE=package.yaml
TEMP_FILE=${TARGET_FILE}.tmp
sed "s/version:             .*/version:             ${LD_RELEASE_VERSION}/" "${TARGET_FILE}" > "${TEMP_FILE}"
mv "${TEMP_FILE}" "${TARGET_FILE}"

TARGET_FILE=launchdarkly-server-sdk.cabal
TEMP_FILE=${TARGET_FILE}.tmp
sed "s/^\(version:\s\+\).*/\1${LD_RELEASE_VERSION}/" "${TARGET_FILE}" > "${TEMP_FILE}"
mv "${TEMP_FILE}" "${TARGET_FILE}"

TARGET_FILE=src/LaunchDarkly/Server/Client/Internal.hs
TEMP_FILE=${TARGET_FILE}.tmp
sed "s/clientVersion = \".*\"/clientVersion = \"${LD_RELEASE_VERSION}\"/" "${TARGET_FILE}" > "${TEMP_FILE}"
mv "${TEMP_FILE}" "${TARGET_FILE}"
