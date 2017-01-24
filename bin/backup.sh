#!/usr/bin/env sh

if [ ${#} -ne 2 ]; then
    echo "Usage: $(basename "${0}") <name of backup> <dir to backup>"
    exit 1
fi

tar_options="cJf"
backup_file_name="${1}"
backup_src_dir="${2}"
backup_dest_dir="${HOME}/Library/Mobile Documents/com~apple~CloudDocs/backups"
backup_dest_file="${backup_dest_dir}/${backup_file_name}.tar.xz"
exclude_file=".exclude"

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

echo "Backing up \"${backup_src_dir}\" to \"${backup_dest_file}\"..."

(
    cd "${backup_src_dir}" || (echo "${backup_src_dir} does not exists!" && exit 1)

    if [ -f $exclude_file ]; then
        tar ${tar_options} "${backup_dest_file}" --exclude-from ${exclude_file}  .
    else
        tar ${tar_options} "${backup_dest_file}" .
    fi
)

tar_exit=$?

if [ $tar_exit -ne 0 ]; then
    echo "Something went wrong."
else
    echo "Done."
fi

exit $tar_exit
