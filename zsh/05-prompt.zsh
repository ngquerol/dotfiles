setprompt() {
    # truncate the PWD to 1/3 of the terminal's width
    ((MAXPWDLEN=($COLUMNS - 1) / 3))

    if [ -z "$SSH_CLIENT" ]; then
        PROMPT="%B%F{green}%$MAXPWDLEN<...<%~%<<%f ${vcs_info_msg_0_}→%b "
    else
        PROMPT="%B%F{yellow}⚡%f %F{blue}%n@%m%f %F{green}%$MAXPWDLEN<...<%~%<<%f ${vcs_info_msg_0_}→%b "
    fi
}
