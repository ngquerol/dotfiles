## login shell configuration

# locale
[ ! -v LANG ] && export LANG=fr_FR.UTF-8

# utilities
export BROWSER=open
export EDITOR=vi

# history
export HISTFILE="${HOME}/.zsh/.cache/zhistory"
export HISTSIZE=10000
export SAVEHIST=10000

# global path arrays
typeset -gU cdpath fpath mailpath path
path+=("${HOME}/.bin")

# machine-dependent configuration
[ -f "${HOME}/.zprofile.local" ] && source "${HOME}/.zprofile.local"
