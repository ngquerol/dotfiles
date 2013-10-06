#!/bin/sh

# Absolute path to this script
SCRIPT=$(readlink -f "$0")
# Absolute path this script is in
SCRIPTPATH=$(dirname "$SCRIPT")

echo -e "\nSymlinking dotfiles...\n"
[ -f $HOME/.zsh ] || ln -s $SCRIPTPATH/zsh ~/.zsh
[ -f $HOME/.zshrc ] || ln -s $SCRIPTPATH/zsh/zshrc ~/.zshrc
[ -f $HOME/.vim  ] || ln -s $SCRIPTPATH/vim ~/.vim
[ -f $HOME/.vimrc  ] || ln -s $SCRIPTPATH/vimrc ~/.vimrc
[ -f $HOME/.gvimrc ] || ln -s $SCRIPTPATH/gvimrc ~/.gvimrc
[ -f $HOME/.gitconfig ] || ln -s $SCRIPTPATH/gitconfig ~/.gitconfig

echo -e "Bootstrapping NeoBundle...\n"
which git > /dev/null
if [ $? -eq 0 ]; then
    git clone git://github.com/Shougo/neobundle.vim.git ~/.vim/bundle/neobundle.vim
fi

echo -e "\nAll set. Restart terminal or source $HOME/.zshrc.\n"
