## Environment variables parsed by zsh

export EDITOR="vim"
export LANG=fr_FR.UTF-8
export BROWSER=firefox
export PAGER="less -M"
export SHELL=/bin/zsh

# History
export HISTFILE=$HOME/.zsh_history
export HISTSIZE=1000
export SAVEHIST=1000

# Check if we are using tmux and/or if there is support for 256 colors
if [ -z "$TMUX" ]; then
    if [ -e /usr/share/terminfo/x/xterm-256color ] ||
        [ -e /usr/share/terminfo/x/xterm+256color ]; then
        export TERM=xterm-256color
    else
        export TERM=xterm
    fi
else
    export TERM=screen-256color
fi
