## shell options

setopt autocd               # if a command is issued that can't be executed as a  normal  command, and the command is the name of a directory, perform the cd command to that directory
setopt complete_in_word     # complete from both ends of a word
setopt correct              # try to correct the history of commands
setopt list_types           # when listing files that are possible completions, show the type of each file with a trailing identifying mark
setopt no_beep              # do not beep
setopt no_check_jobs        # do not check for running jobs upon exit
setopt no_hup               # do not send the HUP signal to running jobs upon exit
setopt prompt_subst         # perform parameter expansion, command substitution and arithmetic expansion in prompts

setopt hist_find_no_dups    # do not enter immediate duplicates into history
setopt hist_ignore_all_dups # if duplicate is to be added, remove older instance in history
setopt hist_ignore_space    # do not add any commands to history that begin with a space

setopt hist_reduce_blanks   # remove  superfluous blanks from each command line being added to the history list.
setopt hist_save_no_dups    # when saving, older commands that duplicate newer commands are omitted

setopt hist_verify          # upon history 'selection', don't execute immediately. Require a carriage return
setopt inc_append_history   # commands are added to the history file immediately upon execution

setopt share_history        # causes all terminals to share the same history 'session'
