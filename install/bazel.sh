#!/bin/sh

# Bazel build tool
echo "Installing Bazel.io (Google Build Tool) prerequesites"
brew install protobuf libarchive
echo "NOTE: for Bazel to work, you still need to install JDK 1.8"
