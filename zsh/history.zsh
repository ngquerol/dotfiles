## history configuration

export HISTFILE="${HOME}/.zsh/.cache/zhistory"
export HISTSIZE=10000
export SAVEHIST=10000

setopt extended_history     # write the history file in the ':start:elapsed;command' format
setopt hist_ignore_space    # do not record an event starting with a space
setopt hist_find_no_dups    # do not enter immediate duplicates into history
setopt hist_ignore_dups     # do not record an event that was just recorded again
setopt hist_ignore_all_dups # if duplicate is to be added, remove older instance in history
setopt hist_ignore_space    # do not add any commands to history that begin with a space
setopt hist_reduce_blanks   # remove superfluous blanks from each command line being added to the history list.
setopt hist_save_no_dups    # when saving, older commands that duplicate newer commands are omitted
setopt hist_verify          # upon history 'selection', don't execute immediately
setopt share_history        # causes all terminals to share the same history 'session'
