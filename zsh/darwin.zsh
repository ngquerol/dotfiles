if [ -x $commands[brew] ]; then
    alias bup="brew update && brew upgrade"
    alias bcl="brew cleanup -s && brew prune -s && brew cask cleanup"
fi
