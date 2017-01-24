#!/usr/bin/env bash

shopt -s extglob

set -o errexit
set -o nounset
set -o pipefail

# Function definitions
check_prerequisites() {
    if ! command -v parallel &>/dev/null; then
        echo >&2 "GNU Parallel is not available. Aborting.";
        exit 1;
    fi

    if ! command -v AtomicParsley &>/dev/null; then
        echo >&2 "AtomicParsley is not available. Aborting.";
        exit 1;
    fi

    if ! command -v ffmpeg &>/dev/null; then
        echo >&2 "FFmpeg is not available. Aborting.";
        exit 1;
    fi
}

process_dir() {
    local targetdir="${1}"
    local outputdir="${2}"
    local cover_file="$(ls "${targetdir}"@(cover|folder).@(jpeg|jpg) 2>/dev/null)"

    parallel --bar convert_track {} \'"${cover_file}"\' "${outputdir}" ::: "${targetdir}"*.flac

    rm -f "${targetdir}"cover-resized-*.jpg # nasty AtomicParsley
}

convert_track() {
    local track="${1}"
    local cover_file="${2}"
    local outputdir="${3}"
    local converted_track="${track%.*}.m4a"
    local embedded_cover="${track%.*}.jpg"

    ffmpeg -hide_banner -nostats -loglevel panic -i "${track}" -y -c:a alac "${converted_track}" < /dev/null

    if [ ! -z "${cover_file}" ]; then
        AtomicParsley "${converted_track}" --artwork "${cover_file}" --overWrite 1>/dev/null
    else
        if ffprobe -hide_banner -loglevel panic -i "${track}" | grep -q "Stream #0:1: Video"; then
            ffmpeg -hide_banner -nostats -loglevel panic -i "${track}" -y -vf \
                   crop="((in_w/2)*2):((in_h/2)*2)" "${embedded_cover}" < /dev/null
            AtomicParsley "${converted_track}" --artwork "${embedded_cover}" --overWrite 1>/dev/null
            rm "{$embedded_cover}"
        fi
    fi
}
export -f convert_track

# Start
if [ ${#} -ne 1 ] && [ ${#} -ne 2 ]; then
    echo "Usage: flac2alac.sh <source dir> [<target dir>]"
    exit 1
fi

# TODO: not compatible with relative paths starting with ".." or "~"
# Alas there is no readlink/realpath binaries installed by default on OSX.
if [ -d "${1}" ]; then
    sourcedir=$([[ ${1} = /* ]] && echo "${1}" || echo "${PWD}/${1#./}")
else
    echo -e >&2 "Could not resolve source directory ${1}. Aborting."
    exit 1
fi

if [ -d "$2" ]; then
    outputdir=$([[ ${2} = /* ]] && echo "${2}" || echo "${PWD}/${2#./}")
else
    outputdir=${sourcedir}
fi

check_prerequisites

# TODO: find a more efficient solution (check once for *.flac per directory)
# Ugly array syntax courtesy of Bash 3.
targetdirs=()
while IFS= read -rd $'\n'; do
    targetdirs+=("${REPLY}")
done < <(find "${sourcedir}" -type f -iname "*.flac" | grep -o '.*/' | sort -u)

targetdirs_count=${#targetdirs[@]}

if [ "${targetdirs_count}" -ne 0 ]; then
    echo -e "\nFound ${targetdirs_count} target(s) containing FLAC files."

    for targetdir in "${targetdirs[@]}"; do
        echo -e "\nProcessing directory ${targetdir}..."
        process_dir "${targetdir}" "${outputdir}"
    done

    echo -e "\nAll done."
else
    echo "No files to convert."
fi

exit 0
