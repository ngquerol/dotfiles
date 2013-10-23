#!/bin/sh

# Absolute path to this script
SCRIPT=$(readlink -f "$0")
# Absolute path this script is in
SCRIPTPATH=$(dirname "$SCRIPT")

echo -e "\nSymlinking dotfiles...\n"
[ -f $HOME/.zsh ] || ln -sv $SCRIPTPATH/zsh $HOME/.zsh
[ -f $HOME/.zshrc ] || ln -sv $SCRIPTPATH/zsh/zshrc $HOME/.zshrc
[ -f $HOME/.vim  ] || ln -sv $SCRIPTPATH/vim $HOME/.vim
[ -f $HOME/.vimrc  ] || ln -sv $SCRIPTPATH/vimrc $HOME/.vimrc
[ -f $HOME/.gitconfig ] || ln -sv $SCRIPTPATH/gitconfig $HOME/.gitconfig

echo -e "\nBootstrapping NeoBundle...\n"
which git > /dev/null
if [ $? -eq 0 ]; then
    git clone git://github.com/Shougo/neobundle.vim.git $HOME/.vim/bundle/neobundle.vim
else
    echo -e "Git wasn't found, aborting."
fi

echo -e "\nAll set. Restart terminal or source $HOME/.zshrc.\n"
