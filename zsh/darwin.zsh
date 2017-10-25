## darwin-specific configuration

# silence 'last login' text
[ ! -f ~/.hushlogin ] && touch ~/.hushlogin

# brew aliases
if [ -x $commands[brew] ]; then
    alias bup="brew update && brew upgrade"
    alias bcl="brew cleanup -s && brew prune -s && brew cask cleanup"
fi

# display man pages formatted as postscript
pman() {
    if [[ (${#} -eq 1) && (-n ${1}) ]]; then
        man -t ${1} | open -f -a /Applications/Preview.app
    else
        print "Usage: pman <man page>"
    fi
}
