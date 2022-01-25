#!/usr/bin/env bash

echo ""
echo "--- Java (OpenJDK)"

brew install openjdk@11

# For the system Java wrappers to find this JDK, symlink it with
echo "Symlinking system Java wrappers to JDK@11"
sudo ln -sfn /usr/local/opt/openjdk@11/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk-11.jdk

echo "--- Installed. (Java OpenJDK)"
