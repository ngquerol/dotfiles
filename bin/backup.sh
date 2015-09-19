#!/usr/bin/env sh

if [ ${#} -ne 2 ]; then
    echo "Usage: $(basename "${0}") <name of backup> <dir to backup>"
    exit 1
fi

backup_file_name="${1}"
backup_src_dir="${2}"
backup_dest_dir="${HOME}/Dropbox/backups"
backup_dest_file="${backup_dest_dir}/${backup_file_name}.tar.xz"
exclude_file=".exclude"

if [ ! -d "${backup_src_dir}" ]; then
    echo "\"${backup_src_dir}\" is not a directory or does not exist."
    exit 1
fi

if [ -f "${backup_dest_file}" ]; then
    while true; do
        read -p "${backup_dest_file} already exists, overwrite? (y/n) " answer
        case $answer in
            [Yy] ) break;;
            [Nn] ) exit 0;;
            * ) echo "Please answer [y]es or [n]o.";;
        esac
    done
fi

echo "Backing up \"${backup_src_dir}\" to \"${backup_dest_file}\"..."

cd "${backup_src_dir}"

if [ -f "${exclude_file}" ]; then
    tar --exclude-from "${exclude_file}" -cf "${backup_dest_file}" * 1>/dev/null
else
    tar -cf "${backup_dest_file}" * 1>/dev/null
fi

tar=$?

cd - 1>/dev/null 2>&1

if [ $tar -ne 0 ]; then
    echo "Something went wrong."
else
    echo "Done."
fi

exit $tar