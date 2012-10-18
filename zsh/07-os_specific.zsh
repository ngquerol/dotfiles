# Distro-specific additions (aliases, completion...)

if [ -f /etc/arch-release ]; then
    if command -v pacman-color >/dev/null 2>&1; then
        alias pacman='sudo pacman-color'
        compdef _pacman pacman-color=pacman
        compdef _pacman yaourt=pacman
    else
        alias pacman='sudo pacman'
    fi
elif [ -f /etc/debian_version ]; then
    alias apt-get='sudo apt-get'
fi
