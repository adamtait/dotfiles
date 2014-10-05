#!/bin/zsh

echo "loading git submodules"
git submodule init
git submodule update


echo "installing elpa packages"
rm -rf $PWD/.emacs.d/elpa
emacs --script "$PWD/install_elpa.el"


echo "creating .path"
 cat > "$HOME/.path" <<EOF
$HOME/bin
/usr/local/bin
/opt/local/bin
/usr/bin
/bin
/usr/sbin
/sbin
/opt/X11/bin
/usr/texbin
EOF


echo "setting up symlinks"
ln -s $PWD/zshrc.symlink ~/.zshrc
ln -s $PWD/emacs.symlink ~/.emacs
ln -s $PWD/slate.symlink ~/.slate
