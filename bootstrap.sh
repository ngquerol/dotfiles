#!/bin/sh

set -o errexit
set -o nounset

script_dir_path=$(cd "$(dirname "${0}")" && pwd)
script_name=$(basename "${0}")
script_path="${script_dir_path}/${script_name}"

# TODO: clobber w/ interactive prompt ?

print_usage() {
    echo "usage: ${script_name} [(l)ink|(u)nlink] (-(-d)ry-run)"
}

link_opt=
unlink_opt=
dryrun_opt=

while [ ${#} -gt 0 ]; do
    case ${1} in
        l|link)
            if [ -n "${unlink_opt}" ]; then
                { print_usage >&2; exit 1; }
            else
                link_opt=1
            fi
            ;;
        u|unlink)
            if [ -n "${link_opt}" ]; then
                { print_usage >&2; exit 1; }
            else
                unlink_opt=1
            fi
            ;;
        -d|--dry-run)
            dryrun_opt=1
            ;;
        *)
            { print_usage >&2; exit 1; }
            ;;
    esac

    shift
done

target() {
    echo "${HOME}/.${1#"${script_dir_path}"/}"
}

is_ours() {
    case $(readlink -f "${1}") in
        "${script_dir_path}"*) true ;;
        *) false ;;
    esac
}

link() {
    for src in "${1}"/*; do
        [ "${src}" = "${script_path}" ] && continue

        dst=$(target "${src}")

        if [ ! -e "${dst}" ]; then
            if [ -n "${dryrun_opt}" ]; then
                echo "ln -snv ${src} ${dst}"
            else
                ln -snv "${src}" "${dst}"
            fi
        elif [ -L "${dst}" ] || [ -f "${dst}" ]; then
            if is_ours "${dst}"; then
                echo "${dst} is already linked, skipping" >&2
            else
                echo "${dst} already exists, skipping" >&2
            fi
        elif [ -d "${dst}" ];then
            link "${src}"
        fi
    done
}

unlink() {
    for src in "${1}"/*; do
        [ "${src}" = "${script_path}" ] && continue

        dst=$(target "${src}")

        if [ -L "${dst}" ] && is_ours "${dst}"; then
            if [ -n "${dryrun_opt}" ]; then
                echo "rm -v ${dst}"
            else
                rm -v "${dst}"
            fi
        elif [ -d "${dst}" ]; then
            unlink "${src}"
        fi
    done
}

if [ -z "${link_opt}" ] && [ -z "${unlink_opt}" ]; then
    { print_usage >&2; exit 1; }
fi

if [ -n "${link_opt}" ]; then
    if [ ! -e "${HOME}/.config" ]; then
        echo "Creating ${HOME}/.config..."
        if [ ! -d "${HOME}/.config" ]; then
            if [ -n "${dryrun_opt}" ]; then
                echo "mkdir ${HOME}/.config"
            else
                mkdir "${HOME}/.config"
            fi
        fi
    fi
    echo "Symlinking dotfiles..."
    link "${script_dir_path}"
fi

if [ -n "${unlink_opt}" ]; then
    echo "Unlinking dotfiles..."
    unlink "${script_dir_path}"
fi

echo "Done."
