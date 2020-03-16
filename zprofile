## login shell configuration

# locale
[ ! -v LANG ] && export LANG=fr_FR.UTF-8
[ ! -v LC_ALL ] && export LC_ALL=fr_FR.UTF-8

# common environment variables
export BROWSER=open
export EDITOR="vi"
export VISUAL="$EDITOR"

# ZSH-related environment variables
export ZSH_CACHE_DIRECTORY="${HOME}/.cache/zsh"

# global path arrays
typeset -gU cdpath fpath mailpath manpath infopath path
path=("${HOME}/.bin" $path)

# machine-dependent configuration
[ -f "${HOME}/.zprofile.local" ] && source "${HOME}/.zprofile.local"
