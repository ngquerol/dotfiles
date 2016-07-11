#!/usr/bin/env sh

# Absolute path this script is in
script_path=$(cd "$(dirname "${0}")"; pwd)

symlink() {
    local source="${1}"
    local target="${HOME}/.$(basename ${1})"

    ln -fsv ${source} ${target}
}

echo "Symlinking files...\n"

symlink ${script_path}/zsh
symlink ${script_path}/zshrc
symlink ${script_path}/bin
symlink ${script_path}/gitconfig
symlink ${script_path}/emacs.d

echo "\nDone."
