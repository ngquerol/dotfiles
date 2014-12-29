#!/usr/bin/env sh

# Absolute path this script is in
SCRIPTPATH=$(cd "$(dirname "$0")"; pwd)

echo -e "Symlinking dotfiles...\n"

[ -f $HOME/.zsh ]       || ln -sv $SCRIPTPATH/zsh $HOME/.zsh
[ -f $HOME/.zshrc ]     || ln -sv $SCRIPTPATH/zsh/zshrc $HOME/.zshrc
[ -f $HOME/.bin ]       || ln -sv $SCRIPTPATH/bin $HOME/.bin
[ -f $HOME/.gitconfig ] || ln -sv $SCRIPTPATH/gitconfig $HOME/.gitconfig
[ -f $HOME/.emacs.d ]   || ln -sv $SCRIPTPATH/emacs.d $HOME/.emacs.d

echo "Done."
