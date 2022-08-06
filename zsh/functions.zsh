## shell functions

# list current 16 ANSI colors
function lscolors_16() {
    _lscolors 16
}

# list current 256 ANSI colors
function lscolors_256() {
    _lscolors 256
}

function _lscolors() {
    local n=${1}

    for i in {0..$(( (n / 8) - 1 ))}; do
        for j in {0..7}; do
            local col=$(( (i * 8) + j ))
            print -Pn "%K{${col}}${(l(2)( )r(2)( ))col}%k"
        done
        print
    done
}

# better man pages
function man() {
  env \
    LESS_TERMCAP_mb=$(printf "\e[1;34m") \
    LESS_TERMCAP_md=$(printf "\e[1;32m") \
    LESS_TERMCAP_me=$(printf "\e[0m") \
    LESS_TERMCAP_se=$(printf "\e[0m") \
    LESS_TERMCAP_so=$(printf "\e[1;47;30m") \
    LESS_TERMCAP_ue=$(printf "\e[0m") \
    LESS_TERMCAP_us=$(printf "\e[0;32m") \
    GROFF_NO_SGR=1 \
    MANPAGER="less -s -M +Gg" \
    man "$@"
}
