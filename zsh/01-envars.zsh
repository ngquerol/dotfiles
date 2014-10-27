## Environment variables parsed by zsh

export EDITOR="emacsclient -t -a ''"
export LANG=fr_FR.UTF-8
export BROWSER=chromium
export SHELL=/bin/zsh

# History
export HISTFILE=$HOME/.zsh_history
export HISTSIZE=10000
export SAVEHIST=10000

# Pretty colors
if command -v sudo 1>/dev/null 2>&1; then
    eval `dircolors -b`
fi
