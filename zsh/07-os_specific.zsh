# Distro-specific additions (aliases, completion...)

if [ -f /etc/debian_version ]; then
    alias apt-get='sudo apt-get'
elif [ -f /etc/arch-release ]; then
    alias pacman='sudo pacman-color'
    compdef _pacman pacman-color = pacman
fi
