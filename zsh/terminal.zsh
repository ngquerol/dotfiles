## terminal-related configuration

# don't do anything if the terminal is very basic
if [[ "${TERM}" == (dumb|linux|*bsd*|eterm*) ]]; then
    return
fi

# look for existing 256 colors support and set ${TERM} accordingly
function set-256colors { 
    if [[ "${TERM}" =~ "-256color$" ]] || [[ "${TERM}" =~ "-kitty$" ]]; then
      return
    fi

    local term_256="${TERM}-256color"

    for file in "${TERMCAP}" "${HOME}/.termcap" "/etc/termcap" "/etc/termcap.small"; do
      if [[ -e "${file}" ]] && grep -E -q "(^${term_256}|\|${term_256})\|" "${file}"; then
        export TERM="${term_256}"
        return
      fi
    done

    for dir in "${TERMINFO}" "${HOME}/.terminfo" "/etc/terminfo" "/lib/terminfo" "/usr/share/terminfo"; do
      if [[ -e "${dir}"/${TERM}[1]/"${term_256}" || -e "${dir}"/"${term_256}" ]]; then
        export TERM="${term_256}"
        return
      fi
    done
}

set-256colors

# set & update the terminal's title
autoload -Uz add-zsh-hook

function set-window-title-pwd() {
    print -Pn "\e]0;%n@%m: %~\007"
}

add-zsh-hook precmd set-window-title-pwd

function set-window-title-cmd() {
    print -Pn "\e]0;%n@%m: ${2}\007"
}

add-zsh-hook preexec set-window-title-cmd
