#!/usr/bin/env bash

set -u
set -e

pgrep "tmux" 1>/dev/null 2>&1 || { echo "tmux is not running!"; exit 1; }

get_tmux_option() {
  local option="$1"
  local default_value="$2"
  local option_value

  option_value="$(tmux show-option -gqv "$option")"

  if [ -z "$option_value" ]; then
    echo "$default_value"
  else
    echo "$option_value"
  fi
}

refresh_interval=$(get_tmux_option "status-interval" "5")
samples_count="60"
os="$(uname -s)"

cpu_usage() {
    case "${os}" in
        "Darwin")
            iostat -w "${refresh_interval}" -c "${samples_count}" \
                | awk 'NR > 2 { printf "cpu: %02d%%\n", 100-$(NF-3) }'
            ;;
        "Linux")
            vmstat -n "$refresh_interval" "$samples_count" \
                | awk 'NR > 2 { printf "cpu: %02d%%\n", 100-$(NF-2) }'
            ;;
        *) echo "??" ;;
    esac
}

mem_stats() {
    case "${os}" in
        "Darwin")
            local page_size
            page_size="$(sysctl -nq "vm.pagesize")"

            vm_stat | awk -v page_size="${page_size}" -F ':' '
    BEGIN { free=0; used=0 }

    /Pages active/ ||
    /Pages wired/ {
      gsub(/^[ \t]+|[ \t]+$/, "", $2); used+=$2;
    }
    /Pages free/ ||
    /Pages inactive/ ||
    /Pages speculative/ ||
    /Pages occupied by compressor/ {
      gsub(/^[ \t]+|[ \t]+$/, "", $2); free+=$2;
    }
    END { print (free * page_size)/1024, (used * page_size)/1024 }
  '
            ;;
        "Linux")
            </proc/meminfo awk '
    BEGIN { total=0; free=0; }
      /MemTotal:/ { total=$2; }

      /MemFree:/ { free+=$2; }
      /Buffers:/ { free+=$2; }
      /Cached:/ { free+=$2; }
      /MemAvailable:/ { free=$2; exit;}
    END { print free, total-free }
  '
            ;;
        *) "??" ;;
    esac
}

mem_usage() {
    mem_stats | awk -v scale="1048576" '{ printf "mem: %.2f/%.2fG", $2/scale, $1/scale + $2/scale }'
}

num_cores() {
    if [ "${os}" == "Darwin" ]; then
        sysctl -nq "hw.ncpu"
    else
        nproc
    fi
}

load_avg(){
    uptime | awk -v "num_cores=$(num_cores)" '{
    sub(/,$/, "", $(NF-2));
    sub(/,$/, "", $(NF-1));
    sub(/,$/, "", $NF);
    printf "load: %.2f %.2f %.2f", $(NF-2)/num_cores, $(NF-1)/num_cores, $NF/num_cores
  }'
}

case "${1}" in
    "cpu") cpu_usage ;;
    "mem") mem_usage ;;
    "load") load_avg ;;
esac
