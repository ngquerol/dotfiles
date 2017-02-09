## ZSH interactive shell configuration

# source zsh completions
if [ -d /usr/local/share/zsh-completions ]; then
    fpath=(/usr/local/share/zsh-completions ${fpath})
fi

# load stuff
autoload -Uz colors && colors
autoload -Uz select-word-style && select-word-style bash
autoload -Uz vcs_info && vcs_info

autoload -Uz bashcompinit && bashcompinit
autoload -Uz compinit

if [ -f ${HOME}/.zsh/.cache/zcompdump ] && [ $(date +'%j') != $(stat -f '%Sm' -t '%j' ${HOME}/.zsh/.cache/zcompdump) ]; then
    compinit -d ${HOME}/.zsh/.cache/zcompdump
else
    compinit -C -d ${HOME}/.zsh/.cache/zcompdump
fi

zmodload zsh/complist

setopt \
    hist_ignore_all_dups \
    hist_ignore_space \
    hist_reduce_blanks \
    share_history \
    prompt_subst \
    nobeep \
    nocheckjobs \
    nohup \
    listtypes \
    extendedglob \
    completeinword \
    completealiases \
    alwaystoend \
    correct \
    autocd

# Source seperate config files
for r in ${HOME}/.zsh/*.zsh; do
    source $r
done
