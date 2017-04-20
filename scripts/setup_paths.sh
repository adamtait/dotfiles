#!/bin/sh

DOTFILES_DIR=$(dirname "$0")/..

echo "creating .path - ${DOTFILES_DIR}"
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

echo "creating .manpath"
cat > "$HOME/.manpath" <<EOF
/usr/local/opt/coreutils/libexec/gnuman
EOF

