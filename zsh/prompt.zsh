autoload -Uz vcs_info

zstyle ":vcs_info:*" enable git
zstyle ":vcs_info:*" check-for-changes true
zstyle ":vcs_info:*" get-revision true
zstyle ":vcs_info:*" stagedstr "%F{green}*%f "
zstyle ":vcs_info:*" unstagedstr "%F{yellow}*%f "
zstyle ":vcs_info:git*" formats "%F{cyan}± %b%f %F{white}%.7i%f %m%c%u"
zstyle ":vcs_info:git*" actionformats "%F{cyan}± %b%f %F{blue}[%a]%f %F{white}%(!.%.7i.)%f %m%c%u"
zstyle ":vcs_info:git*+set-message:*" hooks git-untracked git-aheadbehind git-remotebranch

NEWLINE=$'\n'

PROMPT='${NEWLINE}%B%F{green}$(prompt_dir)%f ${vcs_info_msg_0_}${NEWLINE}%(?.%F{white}.%F{red})→%f%b '

[ -n "${SSH_CLIENT}" ] && PROMPT='%B%F{yellow}⚡%f %F{blue}%n@%m%f%b ${PROMPT}'

RPROMPT='%(1j|%B%F{black}%j %(2j|jobs|job)%f%b|)'

prompt_dir() {
    local max_dir_len=${COLUMNS}
    local current_dir=${PWD/#$HOME/\~}

    if [[ ${#current_dir} -ge max_dir_len ]]; then
        echo -n "${current_dir}" \
            | awk -F '/' '{ print $1 "/" $2 "/…/" $(NF-1) "/" $(NF)}'
    else
        echo "${current_dir}"
    fi
}

+vi-git-untracked() {
    if [[ $(git rev-parse --is-inside-work-tree 2> /dev/null) == "true" ]] && \
           git status --porcelain | grep "??" &> /dev/null; then
        hook_com[unstaged]+="%F{red}*%f "
    fi
}

+vi-git-aheadbehind() {
    local ahead behind
    local -a gitstatus

    behind=$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null | grep -ch "^")
    (( $behind )) && gitstatus+=( "-%F{red}${behind}%f " )

    ahead=$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null | grep -ch "^")
    (( $ahead )) && gitstatus+=( "+%F{green}${ahead}%f " )

    hook_com[misc]+=${gitstatus}

    if [[ -n ${hook_com[misc]} ]]; then
        hook_com[misc]="${hook_com[misc]}"
    fi
}

+vi-git-remotebranch() {
    local remote

    remote=${$(git rev-parse --verify ${hook_com[branch]}@{upstream} \
        --symbolic-full-name 2>/dev/null)/refs\/remotes\/}

    if [[ -n ${remote} && ${remote#*/} != ${hook_com[branch]} ]]; then
        hook_com[branch]="${hook_com[branch]} [${remote}]"
    fi
}
