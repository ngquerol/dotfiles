## prompt customization

precmd_functions+=(render_prompt)

# render the prompt itself
render_prompt() {
    local prompt_newline=$'\n'
    local prompt_character='→'
    local -a preprompt_parts

    # ssh host & username info
    [ -v "${SSH_CLIENT}" ] && preprompt_parts+='%B%F{yellow}⚡%f %F{blue}%n@%m%f%b'

    # current working directory
    preprompt_parts+='%B%F{green}$(prompt_directory)%f%b'

    # VCS status
    preprompt_parts+='${vcs_info_msg_0_}'

    local ps1=(
        ${prompt_newline}
        ${(j. .)preprompt_parts}
        ${prompt_newline}
        "%B%(?.%F{white}.%F{red})${prompt_character} %f%b"
    )

    PROMPT=${(j..)ps1}
}

# show the current working directory, omitting directories if exceeding a certain limit

prompt_directory() {
    local max_dir_len=${COLUMNS}
    local current_dir=${PWD/#$HOME/\~}

    if [[ ${#current_dir} -ge max_dir_len ]]; then
        echo -n "${current_dir}" \
            | awk -F '/' '{ print $1 "/" $2 "/…/" $(NF-1) "/" $(NF)}'
    else
        echo "${current_dir}"
    fi
}

## vcs information retrieval

autoload -Uz vcs_info

precmd_functions+=(vcs_info)

zstyle ":vcs_info:*" enable git
zstyle ":vcs_info:*" check-for-changes true
zstyle ":vcs_info:*" get-revision true
zstyle ":vcs_info:*" stagedstr "%F{green}*%f "
zstyle ":vcs_info:*" unstagedstr "%F{yellow}*%f "
zstyle ":vcs_info:git*" formats "%F{cyan}± %b%f %F{white}%.7i%f %m%c%u"
zstyle ":vcs_info:git*" actionformats "%F{cyan}± %b%f %F{blue}[%a]%f %F{white}%(!.%.7i.)%f %m%c%u"
zstyle ":vcs_info:git*+set-message:*" hooks git-untracked git-aheadbehind git-remotebranch

# show an indicator if there are untracked changes
+vi-git-untracked() {
    if [[ $(git rev-parse --is-inside-work-tree 2> /dev/null) == "true" ]] && \
           git status --porcelain | grep "??" &> /dev/null; then
        hook_com[unstaged]+="%F{red}*%f "
    fi
}

# show how many commits the current branch is ahead/behind relative to the remote
+vi-git-aheadbehind() {
    local ahead behind
    local -a gitstatus

    behind=$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null | grep -ch "^")
    (( $behind )) && gitstatus+=( "%F{red}↓%f${behind} " )

    ahead=$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null | grep -ch "^")
    (( $ahead )) && gitstatus+=( "%F{green}↑%f${ahead} " )

    hook_com[misc]+=${gitstatus}

    if [[ -n ${hook_com[misc]} ]]; then
        hook_com[misc]="${hook_com[misc]}"
    fi
}

# show the name of the remote branch if it differs from the local one
+vi-git-remotebranch() {
    local remote

    remote=${$(git rev-parse --verify ${hook_com[branch]}@{upstream} \
        --symbolic-full-name 2>/dev/null)/refs\/remotes\/}

    if [[ -n ${remote} && ${remote#*/} != ${hook_com[branch]} ]]; then
        hook_com[branch]="${hook_com[branch]} [${remote}]"
    fi
}
