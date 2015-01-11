## Environment variables parsed by zsh

export EDITOR="emacsclient -t "
export ALTERNATE_EDITOR=""
export LANG=fr_FR.UTF-8
export BROWSER=chromium
export SHELL=/bin/zsh

# History
export HISTFILE=$HOME/.zsh_history
export HISTSIZE=10000
export SAVEHIST=10000

# Pretty colors
if [[ $TERM == xterm* ]] && [ -e /usr/share/terminfo/x/xterm-256color ]; then
    export TERM=xterm-256color
fi

if command -v dircolors 1>/dev/null 2>&1; then
    eval `dircolors -b`
fi

export GOPATH=$HOME/Code/Go/Packages
export PATH=$PATH:$HOME/.bin:$GOPATH/bin
