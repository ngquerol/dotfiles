## ZSH login shell configuration

# locale
export LANG=fr_FR.UTF-8

# utilities
export BROWSER=open
export EDITOR=vi

# history
export HISTFILE="${HOME}/.zsh/.cache/zhistory"
export HISTSIZE=10000
export SAVEHIST=10000

# executable path
if [ -d "${HOME}/.bin" ]; then
    export PATH="${HOME}/.bin:${PATH}"
fi

if [ -d "/usr/local/sbin" ]; then
    export PATH="/usr/local/sbin:${PATH}"
fi

if [ -f "${HOME}/.cargo/env" ]; then
    source "${HOME}/.cargo/env"
fi
