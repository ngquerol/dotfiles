## linux-specific shell configuration

status is-interactive; or return

abbr open="xdg-open &>/dev/null"

if [ -f /etc/debian_version ]
    abbr apt-get "sudo apt-get"
    abbr apt "sudo apt"
    abbr aptitude "sudo aptitude"
end

if [ -f /etc/arch-release ]
    abbr pacman "sudo pacman"
end

if [ -f /etc/fedora-release ]
    abbr dnf "sudo dnf"
    abbr yum "sudo yum"
end
