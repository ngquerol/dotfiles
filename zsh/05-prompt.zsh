setprompt() {
    if [ -z "$SSH_CLIENT" ]; then
        PROMPT="%B%F{green}%n@%m%f %F{blue}%~%f ${vcs_info_msg_0_}$%b "
    else
        PROMPT="%B%F{yellow}⚡ %f%F{green}%n@%m%f %F{blue}%~%f ${vcs_info_msg_0_}$%b "
    fi
}
