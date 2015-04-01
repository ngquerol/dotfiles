## Get VCS info, set the prompt and the terminal's title
precmd() {
    vcs_info
    setprompt
    print -Pn "\e]0;%n@%m: %~\a"
}

preexec () {
    print -Pn "\e]0;$1\a"
}

## Coloring man pages
man() {

    env \
        LESS_TERMCAP_mb=$(printf "\e[1;34m") \
        LESS_TERMCAP_md=$(printf "\e[1;32m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;47;30m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[0;32m") \
        man "$@"
}

# Show if there are untracked files in a git repo
+vi-git-untracked() {
    if [[ $(git rev-parse --is-inside-work-tree 2> /dev/null) == 'true' ]] && \
        git status --porcelain | grep '??' &> /dev/null; then
        hook_com[unstaged]+='%F{1}*%f '
    fi
}

# Show how many commits the local branch is ahead or behind the remote branch
+vi-git-aheadbehind() {
    local ahead behind
    local -a gitstatus

    behind=$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null | wc -l)
    (( $behind )) && gitstatus+=( "-%F{red}${behind}%f " )

    ahead=$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null | wc -l)
    (( $ahead )) && gitstatus+=( "+%F{green}${ahead}%f " )

    hook_com[misc]+=${gitstatus}

    if [[ -n ${hook_com[misc]} ]]; then
        hook_com[misc]="${hook_com[misc]}"
    fi
}
