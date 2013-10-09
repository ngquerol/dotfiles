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
