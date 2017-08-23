# look for existing 256 colors support and set $TERM accordingly
if [[ ! "${TERM}" =~ "-256color$" ]]; then
    TERM_256COLORS="${TERM}-256color"
    TERMINFO_DIRECTORIES=(
        "${HOME}/.terminfo"
        "/etc/terminfo"
        "/lib/terminfo"
        "/usr/share/terminfo"
    )

    if [ -x $commands[toe] ] && toe -a 2>/dev/null | grep -q "${TERM_256COLORS}"; then
        export TERM="${TERM_256COLORS}"
    else
        for dir in $TERMINFO_DIRECTORIES; do
            if [ -d "${dir}/${TERM[1]}/${TERM_256COLORS}" ]; then
                export TERM="${TERM_256COLORS}"
                break
            fi
        done
    fi

    unset TERM_256COLORS
    unset TERMINFO_DIRECTORIES
fi

# set & update the terminal's title
set_xterm_title_pwd() {
    print -Pn "\e]0;%n@%m: %~\a"
}

set_xterm_title_cmd() {
    print -Pn "\e]0;%n@%m: ${2}\a"
}

typeset -gU precmd_functions preexec_functions

if [[ $TERM =~ "^xterm" ]]; then
    precmd_functions+=(set_xterm_title_pwd)
    preexec_functions+=(set_xterm_title_cmd)
fi
