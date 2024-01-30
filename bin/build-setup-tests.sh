#!/usr/bin/env bash

# This script is just a quick way to build the setup-tests binary for local development, similar to what is done
# in the Dockerfile.
# It outputs the resulting executable in bin/setup-tests

pushd ./test-setup/ && stack build setup-tests --copy-bins --local-bin-path ../bin/ && popd 
