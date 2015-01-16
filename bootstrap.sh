#!/usr/bin/env sh

# Absolute path this script is in
SCRIPTPATH=$(cd "$(dirname "$0")"; pwd)

echo "Symlinking dotfiles..."

[ -d $HOME/.zsh ]       || ln -sv $SCRIPTPATH/zsh       $HOME/.zsh
[ -f $HOME/.zshrc ]     || ln -sv $SCRIPTPATH/zshrc     $HOME/.zshrc
[ -d $HOME/.bin ]       || ln -sv $SCRIPTPATH/bin       $HOME/.bin
[ -f $HOME/.gitconfig ] || ln -sv $SCRIPTPATH/gitconfig $HOME/.gitconfig
[ -d $HOME/.emacs.d ]   || ln -sv $SCRIPTPATH/emacs.d   $HOME/.emacs.d

echo "Done."
