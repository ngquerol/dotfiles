## darwin-specific configuration

# macOS-specific aliases
alias top="top -o cpu"

# silence 'last login' text
[ ! -f ~/.hushlogin ] && touch ~/.hushlogin

# man pages
manpath=(${(s.:.)"$(manpath)"})

# homebrew configuration & aliases
if [ -x $commands[brew] ]; then
  export HOMEBREW_NO_ANALYTICS=1
  export HOMEBREW_NO_INSECURE_REDIRECT=1
  export HOMEBREW_CASK_OPTS=--require-sha
  export HOMEBREW_PREFIX=$(brew --prefix)

  path=("${HOMEBREW_PREFIX}/bin" "${HOMEBREW_PREFIX}/sbin" $path)
  infopath=("${HOMEBREW_PREFIX}/share/info" $infopath)

  alias bup="brew update && brew upgrade && brew cask upgrade"
  alias bcl="brew cleanup -s"

  # additional completions
  if [ -d "${HOMEBREW_PREFIX}/share/zsh-completions" ]; then
    fpath+=("/usr/local/share/zsh-completions")
  fi
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

