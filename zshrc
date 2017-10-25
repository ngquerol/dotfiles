## interactive shell configuration

# settings modules
files=(
    aliases
    bindkeys
    completion
    functions
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
    source "${HOME}/.zsh/${file}.zsh"
done

# machine-dependent settings
[ -e "${HOME}/.zsh/local.zsh" ] && source "${HOME}/.zsh/local.zsh" 
