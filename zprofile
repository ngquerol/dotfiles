## ZSH login shell configuration

# locale
export LANG=fr_FR.UTF-8

# utilities
export BROWSER=open
export EDITOR=vi

# history
export HISTFILE="~/.zsh/.cache/zhistory"
export HISTSIZE=10000
export SAVEHIST=10000

# executable path
path=(~/.bin $path)

# local & specific configuration
[ -f "~/.zprofile.local" ] && source "~/.zprofile.local"
