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

# Check if we are in X or in a tty, and load colors accordingly
if [ $DISPLAY ]; then
    # Check if rxvt-unicode is installed, if not fall back to xterm
    if command -v urxvt >/dev/null 2>&1; then
        export TERM="rxvt-unicode-256color"
    else
        export TERM="xterm-256color"
    fi
else
    export TERM="linux"
    echo -en "\e]P0000000" #black
    echo -en "\e]P83d3d3d" #darkgrey
    echo -en "\e]P1DE6951" #darkred
    echo -en "\e]P9c56a47" #red
    echo -en "\e]P2bcda55" #darkgreen
    echo -en "\e]PA9dbf60" #green
    echo -en "\e]P3E2A564" #brown
    echo -en "\e]PBEC8A25" #yellow
    echo -en "\e]P42187F6" #darkblue
    echo -en "\e]PC5495DC" #blue
    echo -en "\e]P5875C8D" #darkmagenta
    echo -en "\e]PDE41F66" #magenta
    echo -en "\e]P64390B1" #darkcyan
    echo -en "\e]PE276CC2" #cyan
    echo -en "\e]P7D2D2D2" #lightgrey
    echo -en "\e]PFffffff" #white
    clear #for background artifacting
fi

# Add specific folders to the path
export PATH=$PATH:~/.bin/
