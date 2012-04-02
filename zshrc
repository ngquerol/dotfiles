#==============================================================
#
# CONFIGURATION FOR ZSH
#
# Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>
# 

#=-=-=-=-=-=-=
# load stuffs
#=-=-=-=-=-=-=

autoload -U colors && colors
autoload -U compinit && compinit
autoload -U bashcompinit && bashcompinit

zmodload zsh/complist
zmodload zsh/terminfo

# setopt
setopt \
  autocd \
  extendedglob \
  completeinword \
  always_to_end \
  inc_append_history \
  hist_ignore_all_dups \
  hist_ignore_space \
  hist_reduce_blanks \
  nohup \
  no_beep \
  listtypes \

bindkey -e

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Source seperate config files
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

for r in $HOME/.zsh/*.zsh; do
  if [[ $DEBUG > 0 ]]; then
    echo "zsh: sourcing $r"
  fi
  source $r
done

eval `dircolors -b`
