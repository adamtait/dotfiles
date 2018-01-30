#!/bin/sh

DOTFILES_DIR=$(dirname $(realpath "$0"))/..


if [[ ! -f $DOTFILES_DIR/submodules/bazel/output/bazel ]]; then
    echo "--- compiling Bazel"
    sh $DOTFILES_DIR/submodules/bazel/compile.sh
fi
