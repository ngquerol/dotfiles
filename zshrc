## interactive shell configuration

# settings modules
files=(
    aliases
    bindkeys
    completion
    functions
    local
    options
    prompt
    terminal
)

# os-specific settings
OS=$(uname)

[ -f "${HOME}/.zsh/${OS:l}.zsh" ] && files+="${OS:l}"

unset OS

# source every applicable configuration file
for file in ${files}; do
    [ -f "${HOME}/.zsh/${file}.zsh" ] && source "${HOME}/.zsh/${file}.zsh"
done

