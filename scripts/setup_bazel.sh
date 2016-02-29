#!/bin/zsh

DOTFILES_DIR=$0:a:h/..

if [[ ! -f $DOTFILES_DIR/submodules/bazel/output/bazel ]]; then
    echo "--- compiling Bazel"
    sh $DOTFILES_DIR/submodules/bazel/compile.sh
fi
