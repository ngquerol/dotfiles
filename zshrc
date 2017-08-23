## interactive shell configuration

# settings modules
files=(
    options
    completion
    prompt
    aliases
    functions
    bindkeys
    terminal
)

# os-specific settings
OS=$(uname)

[ -f "${HOME}/.zsh/${OS:l}.zsh" ] && files+="${OS:l}"

unset OS

# source every applicable configuration file
for file in ${files}; do
    source "${HOME}/.zsh/${file}.zsh"
done
