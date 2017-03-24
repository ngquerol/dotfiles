# colored man pages
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

# display man pages formatted as postscript
pman() {
    if [[ (${#} -eq 1) && (-n ${1}) ]]; then
        man -t ${1} | open -f -a /Applications/Preview.app
    else
        print "Usage: pman <man page>"
    fi
}

# git prompt functions
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
