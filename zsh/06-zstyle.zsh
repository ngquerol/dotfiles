# Use cache
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path $HOME/.zsh/temp/zsh_cache

# Remove trailing slashes
zstyle ':completion:*' squeeze-slashes true

# General completion
zstyle ':completion:*' menu select=5
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %m%s
zstyle ':completion:*' group-name ''
zstyle ':completion:*' separate-sections true
zstyle ':completion:*:descriptions' format $'%F{green}completing %B%d%b%f'
zstyle ':completion:*:warnings' format '%F{green}No matches.%f'

# Allow mistakes
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'

# Do not show already selected elements
zstyle ':completion:*:rm:*' ignore-line yes
zstyle ':completion:*:cp:*' ignore-line yes

# Ignore completion functions for commands we don't have
zstyle ':completion:*:functions' ignored-patterns '_*'

# PID completion
zstyle ':completion:*:processes' command ps -A -o pid,user,command
zstyle ':completion:*:processes-names' command 'ps c -u ${USER} -o command | uniq'
zstyle ':completion:*:(killall|pkill|kill):*' menu yes select
zstyle ':completion:*:(killall|pkill|kill):*' force-list always
zstyle ':completion:*:*:kill:*:processes' list-colors "=(#b) #([0-9]#)*=0=01;31"

# Sudo completion
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
       /usr/sbin /usr/bin /sbin /bin

# SCM info in prompt
zstyle ':vcs_info:*' enable git svn
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' get-revision true
zstyle ':vcs_info:*' stagedstr '%F{green}*%f '
zstyle ':vcs_info:*' unstagedstr '%F{yellow}*%f '
zstyle ':vcs_info:git*' formats '%F{cyan}± %b%f %F{black}%7.7i%f %m%c%u'
zstyle ':vcs_info:git*' actionformats '%F{cyan}± %b%f %F{blue}[%a]%f %F{black}%7.7i%f %m%c%u'
zstyle ':vcs_info:git*+set-message:*' hooks git-untracked git-aheadbehind
zstyle ':vcs_info:svn*' formats '%F{cyan}%s %r%f %F{black}r%i%f %m%c%u'

# Colors
zmodload zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
