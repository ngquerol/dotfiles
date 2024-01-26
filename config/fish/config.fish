# disable startup greeting
set fish_greeting ""
set fish_home ~/.config/fish

# source split config files
source $fish_home/environment.fish
source $fish_home/environment.local.fish
source $fish_home/abbrs.fish
source $fish_home/prompt.fish

# os-specific configuration
set -l os_conf ~/.config/fish/(uname | string lower).fish 
test -f $os_conf; and source $os_conf
