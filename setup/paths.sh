#!/usr/bin/env bash

DOTFILES_DIR=$(dirname $(realpath "$0"))/..

echo -e "\n--- creating ~/.path"
cat > "$HOME/.path" <<EOF
/bin
/usr/bin
/sbin
/usr/sbin
/opt/X11/bin
/usr/texbin
$(brew --prefix coreutils)/libexec/gnubin
/usr/local/bin
/opt/local/bin
/usr/local/opt/libarchive/bin
$DOTFILES_DIR/bin
$DOTFILES_DIR/submodules/bazel/output
EOF

echo -e "\n--- creating ~/.manpath"
cat > "$HOME/.manpath" <<EOF
/usr/local/opt/coreutils/libexec/gnuman
EOF

