#!/bin/sh

set -o errexit
set -o nounset

## Global variables

# Absolute path this script is in
script_dir_path=$(cd "$(dirname "${0}")" && pwd)

# Filename of this script
script_name=$(basename "${0}")

# Absolute script path
script_path="${script_dir_path}/${script_name}"

## Functions

link_target() {
    dotfile="${1##*/}"

    echo "${HOME}/.${dotfile}"

    unset dotfile
}

unlink() {
    target="$(link_target "${1}")"

    if [ -L "${target}" ]; then
        [ -n "${verbose_opt}" ] && echo "Removing symlink \"${target}\""
        rm "${target}"
    fi

    unset target
}

link() {
    target="$(link_target "${1}")"

    [ -n "${verbose_opt}" ] && printf "\"%s\" => \"%s\"" \
                                      "${dotfile}" "${target}"

    if [ -n "${clobber_opt}" ]; then
        if [ -n "${verbose_opt}" ] && [ -e "${target}" ]; then
            printf " (clobbering existing target)"
        fi

        ln -Fns "${dotfile}" "${target}"
    else
        if [ -e "${target}" ]; then
            [ -n "${verbose_opt}" ] && \
                printf "\rSkipping \"%s\": target \"%s\" already exists.\n" \
                       "${dotfile}" "${target}"
            return
        fi

        ln -ns "${dotfile}" "${target}"
    fi

    [ -n "${verbose_opt}" ] && printf "\n"

    unset target
}

print_usage() {
    echo "usage: ${script_name} [-(-l)ink|-(-u)nlink] (-(-v)erbose) (-(-f)orce)"
}

## Entry point

link_opt=
unlink_opt=
clobber_opt=
verbose_opt=

while [ $# -gt 0 ]; do
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

if [ -z "${link_opt}" ] && [ -z "${unlink_opt}" ]; then
    print_usage >&2
    exit 1
fi

if [ -n "${link_opt}" ]; then
    echo "Symlinking dotfiles..."
    for dotfile in "${script_dir_path}"/*; do
        if [ -e "${dotfile}" ] && [ "${dotfile}" != "${script_path}" ]; then
            link "${dotfile}"
        fi
    done
    echo "Done."
fi

if [ -n "${unlink_opt}" ]; then
    echo "Unlinking dotfiles..."
    for dotfile in "${script_dir_path}"/*; do
        if [ -e "${dotfile}" ] && [ "${dotfile}" != "${script_path}" ]; then
            unlink "${dotfile}"
        fi
    done
    echo "Done."
fi
