setprompt() {
    if [ -z "$SSH_CLIENT" ]; then
        PROMPT="%B%F{green}%~%f ${vcs_info_msg_0_}→%b "
    else
        PROMPT="%B%F{green}%~%f %F{yellow}⚡ %F{cyan}%n@%m%f ${vcs_info_msg_0_}→%b "
    fi

    RPROMPT="${vcs_info_msg_1_}"
}
