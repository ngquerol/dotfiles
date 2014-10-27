setprompt() {
    if [ -z "$SSH_CLIENT" ]; then
        PROMPT=$'\n'" %B%F{green}%~%f ${vcs_info_msg_0_}→%b "
    else
        PROMPT=" %B%F{yellow}⚡%f %F{blue}%n@%m%f %F{green}%~%f ${vcs_info_msg_0_}→%b "
    fi
}
