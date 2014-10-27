#!/bin/sh

# Absolute path this script is in
SCRIPTPATH=$(cd "$(dirname "$0")"; pwd)

echo -e "\nSymlinking dotfiles...\n"
[ -f $HOME/.zsh ]       || ln -sv $SCRIPTPATH/zsh $HOME/.zsh
[ -f $HOME/.zshrc ]     || ln -sv $SCRIPTPATH/zsh/zshrc $HOME/.zshrc
[ -f $HOME/.emacs.d/ ]  || ln -sv $SCRIPTPATH/emacs.d $HOME/.emacs.d
[ -f $HOME/.gitconfig ] || ln -sv $SCRIPTPATH/gitconfig $HOME/.gitconfig
