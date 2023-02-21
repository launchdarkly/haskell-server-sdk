#!/bin/bash

# The SDK's pcre-light package requires these external dependencies
apt-get update && apt-get install -y libpcre3-dev pkg-config

wget -c https://github.com/phlummox/hup/releases/download/v0.3.0.3/hup-0.3.0.3-linux-amd64.tar.xz -O - | tar -xJ -C /usr/local/bin

stack --no-terminal setup
