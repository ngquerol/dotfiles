# Distro-specific additions (aliases, completion...)

if [ -f /etc/debian_version ]; then
    alias apt-get='sudo apt-get'
fi

if [ -f /etc/arch-release ]; then
    alias pacman='sudo pacman'
fi
