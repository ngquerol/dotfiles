# 256 colors support
if [[ ! "${TERM}" =~ "-256color$" ]]; then
    TERM_256COLORS="${TERM}-256color"
    TERMINFO_DIRECTORIES=(
        "~/.terminfo"
        "/etc/terminfo"
        "/lib/terminfo"
        "/usr/share/terminfo"
    )

    if [ -x $commands[toe] ] && toe -a 2>/dev/null | grep -q "${TERM_256COLORS}"; then
        export TERM="${TERM_256COLORS}"
    else
        for dir in $TERMINFO_DIRECTORIES; do
            if [ -e "${dir}/${TERM[1]}/${TERM_256COLORS}" ]; then
                export TERM="${TERM_256COLORS}"
                break
            fi
        done
    fi

    unset TERM_256COLORS
    unset TERMINFO_DIRECTORIES
fi

# terminal title & VCS info
precmd() {
    vcs_info
    [[ $TERM =~ "^xterm" ]] && print -Pn "\e]0;%n@%m: %~\a"
}

preexec() {
    [[ $TERM =~ "^xterm" ]] && print -Pn "\e]0;%n@%m: ${2}\a"
}
