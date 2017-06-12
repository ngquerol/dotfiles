autoload -Uz vcs_info

zstyle ":vcs_info:*" enable git
zstyle ":vcs_info:*" check-for-changes true
zstyle ":vcs_info:*" get-revision true
zstyle ":vcs_info:*" stagedstr "%F{green}*%f "
zstyle ":vcs_info:*" unstagedstr "%F{yellow}*%f "
zstyle ":vcs_info:git*" formats "%F{cyan}± %b%f %F{white}%.7i%f %m%c%u"
zstyle ":vcs_info:git*" actionformats "%F{cyan}± %b%f %F{blue}[%a]%f %F{white}%(!.%.7i.)%f %m%c%u"
zstyle ":vcs_info:git*+set-message:*" hooks git-untracked git-aheadbehind git-remotebranch

PROMPT='%B%F{green}%(5~|%-1~/…/%3~|%4~)%f ${vcs_info_msg_0_}→%b '

[ -n "${SSH_CLIENT}" ] && PROMPT='%B%F{yellow}⚡%f %F{blue}%n@%m%f%b ${PROMPT}'

RPROMPT='%(1j|%B%F{black}%j %(2j|jobs|job)%f%b|)'
