#!/bin/sh

# Absolute path this script is in
SCRIPTPATH=$(cd "$(dirname "$0")"; pwd)

echo -e "\nSymlinking dotfiles...\n"
[ -f $HOME/.zsh ] || ln -sv $SCRIPTPATH/zsh $HOME/.zsh
[ -f $HOME/.zshrc ] || ln -sv $SCRIPTPATH/zsh/zshrc $HOME/.zshrc
[ -f $HOME/.vim  ] || ln -sv $SCRIPTPATH/vim $HOME/.vim
[ -f $HOME/.vimrc  ] || ln -sv $SCRIPTPATH/vimrc $HOME/.vimrc
[ -f $HOME/.gitconfig ] || ln -sv $SCRIPTPATH/gitconfig $HOME/.gitconfig

echo -e "\nInstalling Vim bundles...\n"

if which git > /dev/null 2>&1; then
    git clone git://github.com/gmarik/Vundle.vim.git $HOME/.vim/bundle/Vundle.vim
    vim -c PluginInstall -c quitall
else
    echo -e "Could not find git. Aborting !"
fi

echo -e "\nAll set. Restart terminal or source $HOME/.zshrc.\n"
