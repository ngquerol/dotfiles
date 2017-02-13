#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

# Function definitions
check_prerequisites() {
    if ! command -v parallel &>/dev/null; then
        echo >&2 "GNU Parallel is not available. Aborting."
        exit 1
    fi

    if ! command -v AtomicParsley &>/dev/null; then
        echo >&2 "AtomicParsley is not available. Aborting."
        exit 1
    fi

    if ! command -v ffmpeg &>/dev/null; then
        echo >&2 "FFmpeg is not available. Aborting."
        exit 1
    fi
}

process_dir() {
    local targetdir="${1}"
    local outputdir="${2}"

    (
        shopt -s extglob nocaseglob

        cd "${targetdir}"

        echo -e "\nProcessing \"${PWD##*/}\"..."

        cover_file="$(ls @(cover|folder).@(jpeg|jpg))"

        parallel --bar convert_track {} \'"${cover_file}"\' \'"${outputdir}"\' ::: *.flac
    )
}
export -f process_dir

convert_track() {
    local track="${1}"
    local cover_file="${2}"
    local outputdir="${3}"
    local converted_track="${track%.*}.m4a"

    ffmpeg -hide_banner -loglevel error -i "${track}" -y -vf \
           crop="((in_w/2)*2):((in_h/2)*2)" -c:a alac "${converted_track}"

    if [ ! -z "${cover_file:-}" ] && [ -f "${cover_file}" ]; then
        AtomicParsley "${converted_track}" --artwork "${cover_file}" \
                      --overWrite 1>/dev/null
    elif ffprobe -hide_banner -loglevel error -i "${track}" | grep -q "Stream #0:1: Video"; then
        local embedded_cover="${track%.*}.jpg"

        ffmpeg -hide_banner -loglevel error -i "${track}" -y -vf \
               crop="((in_w/2)*2):((in_h/2)*2)" "${embedded_cover}"
        AtomicParsley "${converted_track}" --artwork "${embedded_cover}" \
                      --overWrite 1>/dev/null
        rm "{$embedded_cover}"
    fi

    rm -f "*-resized-*.jpg"
}
export -f convert_track

# Start
if [ ${#} -lt 1 ] || [ ${#} -gt 2 ]; then
    echo "Usage: flac2alac.sh <source dir> [<target dir>]"
    exit 1
fi

check_prerequisites

if [ -d "${1}" ]; then
    sourcedir=$(cd "${1}" && pwd)
else
    echo -e >&2 "Could not resolve source directory ${1}. Aborting."
    exit 1
fi

if [ ! -z "${2:-}" ] && [ -d "$2" ]; then
    outputdir=$(cd "${2}" && pwd)
else
    echo "Invalid or unspecified output directory; processing input files in-place."
    outputdir=${sourcedir}
fi

# TODO: find a more efficient solution (check once for *.flac per directory)
# Ugly array syntax courtesy of Bash 3.
targetdirs=()
while IFS= read -rd $'\n'; do
    targetdirs+=("${REPLY}")
done < <(find "${sourcedir}" -type f -iname "*.flac" | grep -o '.*/' | sort -u)

targetdirs_count=${#targetdirs[@]}

if [ "${targetdirs_count}" -ne 0 ]; then
    echo -e "\nFound ${targetdirs_count} directories containing FLAC files."

    for targetdir in "${targetdirs[@]}"; do
        process_dir "${targetdir}" "${outputdir}"
    done

    echo -e "\nAll done."
else
    echo "No files to convert."
fi

exit 0
