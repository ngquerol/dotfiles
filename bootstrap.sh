#!/usr/bin/env bash

set -o errexit
set -o nounset

## Global variables

# Dotfiles (regular files or directories) to manage
dotfiles=(
    "zsh"
    "zshrc"
    "zprofile"
    "zprofile.local"
    "bin"
    "gitconfig"
    "emacs.d"
    "vimrc"
)

# Absolute path this script is in
script_path=$(cd "$(dirname "${0}")" && pwd)

## Functions

link_target() {
    local dotfile
    dotfile=${1}

    echo "${HOME}/.${dotfile}"
}

unlink() {
    local target
    target="$(link_target "${1}")"

    if [ -L "${target}" ]; then
        [ -n "${verbose_opt}" ] && echo "Removing symlink \"${target}\""
        rm "${target}"
    fi
}

link() {
    local target
    target="$(link_target "${1}")"

    [ -n "${verbose_opt}" ] && printf "\"%s\" => \"%s\"" \
                                      "${dotfile}" "${target}"

    if [ -n "${clobber_opt}" ]; then
        if [ -n "${verbose_opt}" ] && [ -e "${target}" ]; then
            printf " (clobbering existing target)"
        fi

        ln -Fns "${script_path}/${dotfile}" "${target}"
    else
        if [ -e "${target}" ]; then
            [ -n "${verbose_opt}" ] && \
                printf "\rSkipping \"%s\": target \"%s\" already exists.\n" \
                       "${dotfile}" "${target}"
            return
        fi

        ln -ns "${script_path}/${dotfile}" "${target}"
    fi

    [ -n "${verbose_opt}" ] && printf "\n"
}

print_usage() {
    echo "usage: ${0} [-(-l)ink|-(-u)nlink] (-(-v)erbose) (-(-f)orce)"
}

## Entry point

link_opt=
unlink_opt=
clobber_opt=
verbose_opt=

while (( $# )); do
    case ${1} in
        -l|--link) [ -n "${unlink_opt}" ] && \
                       { print_usage >&2; exit 1; } || link_opt=1 ;;
        -u|--unlink) [ -n "${link_opt}" ] && \
                         { print_usage >&2; exit 1; } || unlink_opt=1 ;;
        -f|--force) clobber_opt=1 ;;
        -v|--verbose) verbose_opt=1 ;;
        *) print_usage >&2; exit 1 ;;
    esac

    shift
done

if [ -z "$link_opt" ] && [ -z "$unlink_opt" ]; then
    print_usage >&2
    exit 1
fi

if [ -n "$link_opt" ]; then
    echo "Symlinking dotfiles..."
    for dotfile in "${dotfiles[@]}"; do link "${dotfile}"; done
    echo "Done."
fi

if [ -n "$unlink_opt" ]; then
    echo "Unlinking dotfiles..."
    for dotfile in "${dotfiles[@]}"; do unlink "${dotfile}"; done
    echo "Done."
fi
