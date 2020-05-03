## darwin-specific configuration

# silence 'last login' text
[ ! -f ~/.hushlogin ] && touch ~/.hushlogin

# darwin-specific aliases
alias top="top -o cpu"
alias ll="ls -lhFGT"

# homebrew aliases
if [ -x $commands[brew] ]; then
  alias bup="brew update && brew upgrade && brew cask upgrade"
  alias bcl="brew cleanup -s"
fi

# delete local Time Machine snapshots
function tmcleanlocal() {
  local snapshots=(${(f)"$(tmutil listlocalsnapshotdates / | tail +2)"})

  if [ ${#snapshots} -eq 0 ]; then
    echo "No local Time Machine snapshots found."
    return
  fi

  printf "Found %d local snapshot(s):\n\n%s\n\n" "${#snapshots}" "${(pj.\n.)snapshots}"

  if read -q "?Proceed ? "; then
    printf "\n\n"

    for s in $snapshots; do
      printf "Deleting local Time Machine snapshot \"%s\"...\n" "${s}"
      tmutil deletelocalsnapshots ${s} &>/dev/null
    done
  fi
}

# display man pages formatted as postscript
function pman() {
  if [[ (${#} -eq 1) && (-n ${1}) ]]; then
    man -t ${1} | open -f -a /Applications/Preview.app
  else
    print "Usage: pman <man page>"
  fi
}
