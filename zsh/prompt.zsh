## prompt customization

## vcs information retrieval

autoload -Uz vcs_info
autoload -Uz add-zsh-hook

add-zsh-hook precmd vcs_info

zstyle ":vcs_info:*" enable git
zstyle ":vcs_info:*" check-for-changes true
zstyle ":vcs_info:*" get-revision true
zstyle ":vcs_info:*" stagedstr "%F{green}*%f"
zstyle ":vcs_info:*" unstagedstr "%F{yellow}*%f"
zstyle ":vcs_info:git*" formats "%F{cyan}± %b%f %F{white}%.7i%f %m%c%u"
zstyle ":vcs_info:git*" actionformats "%F{cyan}± %b%f %F{yellow}(%a)%f"
zstyle ":vcs_info:git*+set-message:*" hooks git-untracked git-aheadbehind git-remotebranch git-stash

# show an indicator if there are untracked files
function +vi-git-untracked() {
    if [[ ! $(git ls-files --others --exclude-standard) ]]; then
        return
    fi

    hook_com[unstaged]+="%F{red}*%f"
}

# show how many commits the current branch is ahead/behind relative to the remote
function +vi-git-aheadbehind() {
    local ahead behind
    local -a gitstatus

    behind=$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null | grep -ch "^")
    (( $behind )) && gitstatus+=( "%F{red}↓%f${behind} " )

    ahead=$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null | grep -ch "^")
    (( $ahead )) && gitstatus+=( "%F{green}↑%f${ahead} " )

    hook_com[misc]+=${gitstatus}
}

# show the name of the remote branch if it differs from the local one
function +vi-git-remotebranch() {
    local remote

    remote=${$(git rev-parse --verify ${hook_com[branch]}@{upstream} \
                   --symbolic-full-name 2>/dev/null)/refs\/remotes\/}

    if [[ -n ${remote} && ${remote#*/} != ${hook_com[branch]} ]]; then
        hook_com[branch]="${hook_com[branch]} [${remote}]"
    fi
}

# show count of stashed changes
function +vi-git-stash() {
    if [[ ! -s "${vcs_comm[gitdir]}/logs/refs/stash" ]]; then
        return
    fi

    local -a stashes=(${(f)"$(<${vcs_comm[gitdir]}/logs/refs/stash)"})

    if [ ${#stashes} -gt 0 ]; then
        hook_com[misc]+="%F{magenta}$%f"
    fi
}

# print a newline before the prompt, unless it's the first prompt in the process
function print_newline() {
    if [ -z "$NEWLINE_BEFORE_PROMPT" ]; then
        NEWLINE_BEFORE_PROMPT=1
    elif [ "$NEWLINE_BEFORE_PROMPT" -eq 1 ]; then
        echo ""
    fi
}

add-zsh-hook precmd print_newline

# abbreviate path (fish-style) if exceeding a certain length
function prompt_pwd() {
  local length_limit=$(( $COLUMNS * 0.4 ))
  local path_array=(${(s:/:)${PWD/${HOME}/\~}})

  for (( i = 1; ${#${(j:/:)path_array}} > $length_limit && i < $#path_array; i++ )) do
    path_array[i]="${path_array[i]:0:1}"
  done

  if [[ ${path_array[1]} != \~ ]]; then
    path_array=("" $path_array)
  fi

  echo ${(j:/:)path_array}
}

# render the prompt itself
function render_prompt() {

    # left prompt
    local -a left_prompt

    # ssh host & username info
    [ -v "${SSH_CLIENT}" ] && left_prompt+='%B%F{yellow}⚡%f %F{blue}%n@%m%f%b'

    # current working directory
    left_prompt+='%B%F{green}$(prompt_pwd)%f%b'

    # arrow, red if last exit code != 0
    left_prompt+='%B%(?.%F{white}.%F{red})→%f%b '

    PROMPT=${(j. .)left_prompt}

    # right prompt
    local -a right_prompt

    # background jobs
    right_prompt+='%F{yellow}%(1j.%jj.)%f'

    # VCS info
    right_prompt+='${vcs_info_msg_0_}'

    RPROMPT=${(j. .)right_prompt}
}

add-zsh-hook precmd render_prompt
