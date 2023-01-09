#!/bin/bash

# The SDK's pcre-light package requires these external dependencies
apt-get update && apt-get install -y libpcre3-dev pkg-config

stack --no-terminal setup
