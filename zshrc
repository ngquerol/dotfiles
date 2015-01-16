#
# ZSH configuration file
#
# Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>
#

# load stuff
autoload -Uz colors && colors
autoload -Uz compinit && compinit -d $HOME/.zsh/temp/zcompdump
autoload -Uz bashcompinit && bashcompinit
autoload -Uz select-word-style && select-word-style bash
autoload -Uz vcs_info && vcs_info

zmodload zsh/complist
zmodload zsh/terminfo

setopt \
    hist_ignore_all_dups \
    hist_ignore_space \
    hist_reduce_blanks \
    prompt_subst \
    nobeep \
    nocheckjobs \
    nohup \
    listtypes \
    extendedglob \
    completeinword \
    alwaystoend \
    correct \
    autocd

# Source seperate config files
for r in $HOME/.zsh/*.zsh; do
    source $r
done
