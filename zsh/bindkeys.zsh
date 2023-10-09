## global shell keybindings

# bash word selection style
autoload -U select-word-style && select-word-style bash

# emacs-style keybindings
bindkey -e

case ${TERM} in
    xterm*)
        bindkey "\e[H"    beginning-of-line     # Home
        bindkey "\e[F"    end-of-line           # End
        bindkey "\e[5~"   beginning-of-history  # PageUp
        bindkey "\e[6~"   end-of-history        # PageDown
        bindkey "\e[2~"   quoted-insert         # Ins
        bindkey "\e[3~"   delete-char           # Del
        bindkey "\e[1;5C"  forward-word         # Ctrl-Right
        bindkey "\e[1;5D"  backward-word        # Ctrl-Left
        bindkey "\e[1;3C" emacs-forward-word    # Alt-Right
        bindkey "\e[1;3D" emacs-backward-word   # Alt-Left
        bindkey "\C-w"    kill-region           # Ctrl-w
        bindkey "^[[Z"    reverse-menu-complete # Shift-TAB
        ;;
    linux*)
        bindkey "\e[1~" beginning-of-line      # Home
        bindkey "\e[4~" end-of-line            # End
        bindkey "\e[5~" beginning-of-history   # PageUp
        bindkey "\e[6~" end-of-history         # PageDown
        bindkey "\e[2~" quoted-insert          # Ins
        bindkey "\e[3~" delete-char            # Del
        ;;
esac
