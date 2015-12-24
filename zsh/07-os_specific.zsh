# Distro-specific additions (aliases, completion...)
if [[ $(uname) == "Linux" ]]; then
    if [ -f /etc/debian_version ]; then
        alias apt-get='sudo apt-get'
    fi

    if [ -f /etc/arch-release ]; then
        alias pacman='sudo pacman'
    fi

    if [ -f /etc/fedora-release ]; then
        alias dnf='sudo dnf'
        alias yum='sudo yum'
    fi
fi
