## ZSH login shell configuration

# locale
export LANG=fr_FR.UTF-8

# utilities
export BROWSER=open
export EDITOR=vi

# history
export HISTFILE="${HOME}/.zsh/.cache/zhistory"
export HISTSIZE=10000
export SAVEHIST=10000

# executable path
export PATH="${HOME}/.bin:${PATH}"

# local & specific configuration
[ -f "${HOME}/.zprofile.local" ] && source "${HOME}/.zprofile.local"
