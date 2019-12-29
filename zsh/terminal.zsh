## terminal-related configuration

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

if [[ "${TERM}" == (dumb|linux|*bsd*|eterm*) ]]; then
  return
fi

autoload -Uz add-zsh-hook

function set-window-title-pwd() {
    print -Pn "\e]0;%n@%m: %~\a"
}

add-zsh-hook precmd set-window-title-pwd

function set-window-title-cmd() {
    print -Pn "\e]0;%n@%m: ${2}\a"
}

add-zsh-hook preexec set-window-title-cmd
