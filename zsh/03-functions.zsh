## Title & git stuff
precmd() {

    case $TERM in
        xterm*)
            print -Pn "\e]0;%n@%m : %~\a" ;;
    esac

    vcs_info

    setprompt
}

preexec() {

    case $TERM in
        xterm*)
            print -Pn "\e]0;$1\a" ;;
    esac
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

# Show if there is untracked files in a git repo
+vi-git-untracked() {

if [[ $(git rev-parse --is-inside-work-tree 2> /dev/null) == 'true' ]]; then
    if [[ $(git status --porcelain | grep '??' &> /dev/null) == 'true' ]]; then
        hook_com[unstaged]+='%F{1}●%f '
    fi
fi
}

# Show how many commits are behind/ahead the remote branch
+vi-git-aheadbehind() {
    local ahead behind
    local -a gitstatus

    # for git prior to 1.7
    # ahead=$(git rev-list origin/${hook_com[branch]}..HEAD | wc -l)
    ahead=$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null | wc -l)
    (( $ahead )) && gitstatus+=( "%F{blue}(%F{green}+${ahead}%F{blue})%f " )

    # for git prior to 1.7
    # behind=$(git rev-list HEAD..origin/${hook_com[branch]} | wc -l)
    behind=$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null | wc -l)
    (( $behind )) && gitstatus+=( "%F{blue}(%F{red}(-${behind}%F{blue})%f " )

    hook_com[misc]+=${(j::)gitstatus}
}
