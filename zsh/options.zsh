## shell options

setopt autocd               # if a command is issued that can't be executed as a normal command, and the command is the name of a directory, perform the cd command to that directory
setopt complete_in_word     # complete from both ends of a word
setopt correct              # try to correct the history of commands
setopt list_types           # when listing files that are possible completions, show the type of each file with a trailing identifying mark
setopt no_beep              # do not beep
setopt no_check_jobs        # do not check for running jobs upon exit
setopt no_hup               # do not send the HUP signal to running jobs upon exit
setopt prompt_subst         # perform parameter expansion, command substitution and arithmetic expansion in prompts
