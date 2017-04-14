#!/usr/bin/env sh

set -o errexit
set -o nounset

# Absolute path this script is in
script_path=$(cd "$(dirname "${0}")" && pwd)

symlink() {
    source="${1}"
    target="${HOME}/.$(basename "${1}")"

    ln -fsv "${source}" "${target}"
}

printf "Symlinking files...\n"

symlink "${script_path}/zsh"
symlink "${script_path}/zshrc"
symlink "${script_path}/zprofile"
symlink "${script_path}/zprofile.local"
symlink "${script_path}/bin"
symlink "${script_path}/gitconfig"
symlink "${script_path}/emacs.d"

printf "Done.\n"
