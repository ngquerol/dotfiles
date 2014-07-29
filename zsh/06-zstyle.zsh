# Main
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no result for : %d%b'
zstyle ':completion:*' menu select=2
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s

# Remove trailing slashes
zstyle ':completion:*' squeeze-slashes true

# Use cache
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh_cache

# Allow mistakes
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
#zstyle ':completion:*:approximate:*' max-errors 1 numeric
zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'

# [?] Ignore completion functions for commands we don't have
zstyle ':completion:*:functions' ignored-patterns '_*'

# Colors
# You can also add different colours to the completion list - as displayed in
# the screenshot below. To be more specific, we'll use the same colours that
# GNU ls shows with the --color option
zmodload zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Do not show already selected elements
zstyle ':completion:*:rm:*' ignore-line yes
zstyle ':completion:*:cp:*' ignore-line yes

# PID completion
zstyle ':completion:*:*:kill:*'              menu yes select
zstyle ':completion:*:kill:*'                force-list always
zstyle ':completion:*:*:kill:*:processes'    list-colors "=(#b) #([0-9]#)*=0=01;31"
zstyle ':completion:*:processes'             command 'ps -axw'
zstyle ':completion:*:processes-names'       command 'ps -awxho command'

# Sudo completion
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
    /usr/sbin /usr/bin /sbin /bin

# SCM info in prompt
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' get-revision true
zstyle ':vcs_info:*' stagedstr '%F{2}*%f '
zstyle ':vcs_info:*' unstagedstr '%F{3}*%f '
zstyle ':vcs_info:git*' formats '%F{blue}± %b%f %F{black}%7.7i%f %m %c%u'
zstyle ':vcs_info:git*' actionformats '%F{blue}± %b%f %F{7}[%a]%f %F{black}%7.7i%f %c%u'
zstyle ':vcs_info:git*+set-message:*' hooks git-untracked git-aheadbehind

