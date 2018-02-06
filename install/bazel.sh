#!/usr/bin/env bash

# Bazel build tool
echo -e "\n--- Bazel.io (Google Build Tool) prerequesites"
brew install protobuf libarchive

echo -e "\n---------------------------"
echo "NOTE: for Bazel to work, you still need to install JDK 1.8"
echo -e "---------------------------\n"
