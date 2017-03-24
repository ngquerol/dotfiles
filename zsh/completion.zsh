autoload -Uz compinit
autoload -Uz bashcompinit

if [ -d /usr/local/share/zsh-completions ]; then
    fpath=(/usr/local/share/zsh-completions ${fpath})
fi

# rebuild completion cache only if it hasn't been done in the last 24 hours
if [[ -n ${HOME}/.zsh/.cache/zcache(#qN.mh+24) ]]; then
    compinit -d ${HOME}/.zsh/.cache/zcache
else
    compinit -C -d ${HOME}/.zsh/.cache/zcache
fi

bashcompinit

# completion cache
zstyle ":completion:*" cache-path ${HOME}/.zsh/.cache/zcache
zstyle ":completion::complete:*" use-cache true

# completions list
zstyle ":completion:*" menu select=long-list select=1
zstyle ":completion:*" select-prompt "%B%F{green}%m matches%f%b"
zstyle ":completion:*:descriptions" format "%F{green}completing %B%d%b%f"
zstyle ":completion:*:warnings" format "%F{green}No matches found.%f"
zstyle ":completion:*" group-name ""

# completions list colors
if [ -n "${LS_COLORS}" ]; then
    zmodload zsh/complist
    zstyle ":completion:*:" list-colors ${(s.:.)LS_COLORS}
fi

# separate directories from files
zstyle ":completion:*" list-dirs-first true

# remove trailing slashes
zstyle ":completion:*" squeeze-slashes true

# allow mistakes
zstyle ":completion:*" completer _complete _match _approximate
zstyle ":completion:*:match:*" original only
zstyle ":completion:*:approximate:*" max-errors "reply=($((($#PREFIX+$#SUFFIX)/3))numeric)"

# do not try to complete exact matches anyway
zstyle ":completion:*" accept-exact "*(N)"
zstyle ":completion:*" accept-exact-dirs true

# do not show already selected elements
zstyle ":completion:*:rm:*" ignore-line yes
zstyle ":completion:*:cp:*" ignore-line yes

# ignore completion functions for commands we don't have
zstyle ":completion:*:functions" ignored-patterns "_*"

# man pages completion
zstyle ":completion:*:manuals" separate-sections true
zstyle ":completion:*:manuals.*" insert-sections true
zstyle ":completion:*:man:*" menu yes select

# process completion
zstyle ":completion:*:processes" command "ps -cxo pid,user,%cpu,%mem,command"
zstyle ":completion:*:processes-names" command "ps -aeo command"
zstyle ":completion:*:*:kill:*:processes" list-colors "=(#b) #([0-9]#)*=0=01;31"
zstyle ":completion:*:processes" menu yes select
zstyle ":completion:*:processes-names" menu yes select

# sudo completion
zstyle ":completion:*:sudo:*" command-path ${(s.:.)PATH}

# automatically rehash commands
zstyle ":completion:*" rehash true
