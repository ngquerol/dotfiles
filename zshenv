## shell environment configuration

# current OS
case $(uname | tr '[:upper:]' '[:lower:]') in
  linux*) export ZSH_OS_NAME=linux ;;
  freebsd*) export ZSH_OS_NAME=freebsd ;;
  darwin*) export ZSH_OS_NAME=darwin ;;
  msys*) export ZSH_OS_NAME=windows ;;
  cygwin*) export ZSH_OS_NAME=windows ;;
  *) export ZSH_OS_NAME=unknown ;;
esac

# locale
[ ! -v LANG ] && export LANG=fr_FR.UTF-8
[ ! -v LC_ALL ] && export LC_ALL=fr_FR.UTF-8

# common environment variables
export BROWSER="open"
export EDITOR="vi"
export VISUAL="$EDITOR"
export PAGER="less -s -M +Gg"

# ZSH-related environment variables
export ZSH_CACHE_DIRECTORY="${HOME}/.cache/zsh"
