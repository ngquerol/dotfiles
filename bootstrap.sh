#!/bin/sh

# Absolute path to this script
SCRIPT=$(readlink -f "$0")
# Absolute path this script is in
SCRIPTPATH=$(dirname "$SCRIPT")

[ -f $HOME/.zsh ] || ln -s $SCRIPTPATH/zsh ~/.zsh
[ -f $HOME/.zshrc ] || ln -s $SCRIPTPATH/zshrc ~/.zshrc
[ -f $HOME/.vim  ] || ln -s $SCRIPTPATH/vim ~/.vim
[ -f $HOME/.vimrc  ] || ln -s $SCRIPTPATH/vimrc ~/.vimrc
[ -f $HOME/.gitconfig ] || ln -s $SCRIPTPATH/gitconfig ~/.gitconfig

# Bootstrap NeoBundle
which git > /dev/null
if [ $? -eq 0 ]; then
    git clone git://github.com/Shougo/neobundle.vim.git ~/.vim/bundle/neobundle.vim
fi
