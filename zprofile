## Environment variables

# Utilities
export LANG=fr_FR.UTF-8
export BROWSER=open
export EDITOR="emacs -nw -Q"

# ZSH history
export HISTFILE=$HOME/.zsh/.cache/zhistory
export HISTSIZE=10000
export SAVEHIST=10000

# Terminal colors
if [[ $TERM == xterm* ]] && [ -e /usr/share/terminfo/78/xterm-256color ]; then
    export TERM=xterm-256color
fi

if command -v dircolors 1>/dev/null 2>&1; then
    eval `dircolors -b`
fi

# Executable path
export PATH=$HOME/.bin:/usr/local/sbin:$PATH
