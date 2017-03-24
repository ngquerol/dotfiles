# 256 colors support
if [[ ! $TERM =~ "-256color$" ]]; then
    TERM_256COLORS="${TERM}-256color"
    TERMINFO_DIRECTORIES=(
        "${HOME}/.terminfo"
        "/etc/terminfo"
        "/lib/terminfo"
        "/usr/share/terminfo"
    )

    if [ -x $commands[toe] ] && toe -a | grep -q "${TERM_256COLORS}"; then
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
if [[ $TERM =~ "^xterm" ]]; then
    precmd() {
        vcs_info
        print -Pn "\e]0;%n@%m: %~\a"
    }

    preexec() {
        print -Pn "\e]0;%n@%m: $1\a"
    }
fi
