#!/usr/bin/env bash

DOTFILES_DIR=$(dirname $(realpath "$0"))/..

echo ""
echo "--- creating ~/.path"
cat > "$HOME/.path" <<EOF
$(brew --prefix coreutils)/libexec/gnubin
$DOTFILES_DIR/bin
$HOME/workspace/google-cloud-sdk/bin
/opt/homebrew/bin
/usr/local/bin
/opt/local/bin
/usr/local/opt/libarchive/bin
/opt/X11/bin
/usr/texbin
/usr/sbin
/usr/bin
/sbin
/bin
EOF

echo ""
echo "--- creating ~/.manpath"
cat > "$HOME/.manpath" <<EOF
/usr/local/opt/coreutils/libexec/gnuman
EOF

