## ZSH interactive shell configuration

files=(
    options
    completion
    prompt
    aliases
    functions
    bindkeys
    terminal
)

OS=$(uname)

[ -f "${HOME}/.zsh/${OS:l}.zsh" ] && files+="${OS:l}"

unset OS

for file in ${files}; do
    source "${HOME}/.zsh/${file}.zsh"
done
