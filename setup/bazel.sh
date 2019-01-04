#!/usr/bin/env bash

DOTFILES_DIR=$(dirname $(realpath "$0"))/..


if [[ ! -f $DOTFILES_DIR/submodules/bazel/output/bazel ]]; then
    echo ""
    echo "--- compiling Bazel"
    sh $DOTFILES_DIR/submodules/bazel/compile.sh
fi
