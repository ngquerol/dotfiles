setprompt() {
    if [ -z "$SSH_CLIENT" ]; then
        PROMPT="%B%F{green}%~%f %F{blue}${vcs_info_msg_0_}%f→%b "
    else
        PROMPT="%B%F{green}%~%f %F{cyan}%F{yellow}⚡ %F{cyan}%n@%m %F{blue}${vcs_info_msg_0_}%f→%b "
    fi

        RPROMPT="${vcs_info_msg_1_}"
}
