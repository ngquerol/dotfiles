## interactive shell configuration

# zmodload zsh/zprof

# settings modules
files=(
    aliases
    bindkeys
    completion
    functions
    history
    options
    prompt
    terminal
)

# os-specific settings
OS=$(uname)

[ -f "${HOME}/.zsh/${OS:l}.zsh" ] && files+="${OS:l}"

unset OS

# create support directories, if necessary
[ -d ${ZSH_CACHE_DIRECTORY} ] || mkdir -p "${ZSH_CACHE_DIRECTORY}"

# source every applicable configuration file
for file in ${files}; do
    [ -f "${HOME}/.zsh/${file}.zsh" ] && source "${HOME}/.zsh/${file}.zsh"
done

# zprof
