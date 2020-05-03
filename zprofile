## login shell configuration

# OS-dependent configuration
[ -f "${HOME}/.zprofile.${ZSH_OS_NAME}" ] && source "${HOME}/.zprofile.${ZSH_OS_NAME}"

# global path arrays
typeset -gU cdpath fpath mailpath manpath infopath path
path=("${HOME}/.bin" $path)
manpath=(${(s.:.)"$(man --path)"})
