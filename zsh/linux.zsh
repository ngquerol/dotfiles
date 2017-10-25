## linux-specific configuration

alias open="xdg-open &>/dev/null"

if [ -f /etc/debian_version ]; then
    alias apt-get="sudo apt-get"
    alias apt="sudo apt"
    alias aptitude="sudo aptitude"
elif [ -f /etc/arch-release ]; then
    alias pacman="sudo pacman"
elif [ -f /etc/fedora-release ]; then
    alias dnf="sudo dnf"
    alias yum="sudo yum"
fi
