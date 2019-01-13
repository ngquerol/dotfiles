## darwin-specific configuration

# macOS-specific aliases
alias top="top -o cpu"

# silence 'last login' text
[ ! -f ~/.hushlogin ] && touch ~/.hushlogin

# brew aliases
if [ -x $commands[brew] ]; then
    path=("/usr/local/bin" "/usr/local/sbin" $path)
    alias bup="brew update && brew upgrade"
    alias bcl="brew cleanup -s"
fi

# display man pages formatted as postscript
pman() {
    if [[ (${#} -eq 1) && (-n ${1}) ]]; then
        man -t ${1} | open -f -a /Applications/Preview.app
    else
        print "Usage: pman <man page>"
    fi
}
