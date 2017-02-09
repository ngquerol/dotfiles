#!/usr/bin/env sh

set -o errexit
set -o nounset
set -o pipefail

if [ ${#} -ne 2 ]; then
    echo "Usage: $(basename "${0}") <name of backup> <dir to backup>"
    exit 1
fi

compression_program="xz --threads=0"
backup_file_name="${1}"
backup_src_dir="${2}"
backup_dest_dir="${HOME}/Library/Mobile Documents/com~apple~CloudDocs/backups"
backup_dest_file="${backup_dest_dir}/${backup_file_name}.tar.xz"
exclude_file="${backup_src_dir}/.exclude"

if [ ! -d "${backup_src_dir}" ]; then
    echo >&2 "Directory \"${backup_src_dir}\" does not exists, aborting."
    exit 1
fi

echo "Backing up \"${backup_src_dir}\" to \"${backup_dest_file}\"..."

if [ -f "${backup_dest_file}" ]; then
    printf "%s already exists, overwrite? (y/n) " "${backup_dest_file}"

    while true; do
        read -r answer

        case $answer in
            [Yy] ) break;;
            [Nn] ) exit 0;;
            * ) echo "Please answer [y]es or [n]o.";;
        esac
    done
fi

if [ -f "${exclude_file}" ]; then
    tar --exclude-from "${exclude_file}" \
        -cpf - "${backup_src_dir}" \
        | ${compression_program} > "${backup_dest_file}"
else
    tar -cpf - "${backup_src_dir}" \
        | ${compression_program} > "${backup_dest_file}"
fi

tar_exit=$?

if [ $tar_exit -ne 0 ]; then
    echo >&2 "Something went wrong."
else
    echo "Done."
fi

exit $tar_exit
