#!/bin/bash

set -e

# Upload both the distribution file and the associated documentation to Hackage
HUP_HACKAGE_PASSWORD="$(cat "${LD_RELEASE_SECRETS_DIR}/hup_hackage_password")"
export HUP_HACKAGE_PASSWORD

hup packboth -u launchdarkly
