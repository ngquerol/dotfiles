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
[ -f "${HOME}/.zsh/${ZSH_OS_NAME}.zsh" ] && files+="${ZSH_OS_NAME}"

# create support directories, if necessary
[ -d ${ZSH_CACHE_DIRECTORY} ] || mkdir -p "${ZSH_CACHE_DIRECTORY}"

# source every applicable configuration file
for file in ${files}; do
    [ -f "${HOME}/.zsh/${file}.zsh" ] && source "${HOME}/.zsh/${file}.zsh"
done

# zprof
