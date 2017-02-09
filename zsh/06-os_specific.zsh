# OS/Distro-specific additions (aliases, completion...)
OS=$(uname)

if [[ $OS = "Linux" ]]; then
    if [ -f /etc/debian_version ]; then
        alias apt-get='sudo apt-get'
        alias apt='sudo apt'
        alias aptitude='sudo aptitude'
    fi

    if [ -f /etc/arch-release ]; then
        alias pacman='sudo pacman'
    fi

    if [ -f /etc/fedora-release ]; then
        alias dnf='sudo dnf'
        alias yum='sudo yum'
    fi
elif [[ $OS = "Darwin" ]]; then
    alias bup="brew update && brew upgrade"
    alias bcl="brew cleanup -s && brew prune -s && brew cask cleanup"
fi

unset OS
