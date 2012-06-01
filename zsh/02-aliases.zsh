# RM
alias rm='rm -r'
alias rmf='rm -rf'

# CP
alias cp='cp -ri'

# LS
alias ls='ls -Xp --color=auto'
alias ll='ls -Xplh --color=auto'

# MKDIR
alias mk='mkdir -p'
alias mkdir='mkdir -p'

# GREP
alias grep='grep --color=auto'

# VIM
alias v='vim'

# Distro-specific aliases
if [ -f /etc/arch-release ]; then
    if command -v pacman-color >/dev/null 2>&1; then
        alias pacman='sudo pacman-color'
    else
        alias pacman='sudo pacman'
    fi
elif [ -f /etc/debian_version ]; then
    alias apt-get='sudo apt-get'
fi
