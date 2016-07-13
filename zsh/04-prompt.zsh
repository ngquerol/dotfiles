setprompt() {

    # While this might look like an invocation of Chtuhlu, in reality it
    # checks if the pwd is longer than 5 elements, and truncates it down
    # to the 1st + the 3 last elements if applicable.
    # Also shows some SSH / VCS info when the need arises.
    if [ -z "$SSH_CLIENT" ]; then
        PROMPT="%B%F{green}%(5~|%-1~/…/%3~|%~)%f ${vcs_info_msg_0_}→%b "
    else
        PROMPT="%B%F{yellow}⚡%f %F{blue}%n@%m%f %F{green}%(5~|%-1~/…/%3~|%~)%f ${vcs_info_msg_0_}→%b "
    fi
}
