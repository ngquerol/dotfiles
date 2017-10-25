## global shell keybindings

# emacs-style keybindings
autoload -U select-word-style && select-word-style bash

bindkey -e

case $TERM in
    xterm*)
        bindkey "\e[H"    beginning-of-line    # Home
        bindkey "\e[F"    end-of-line          # End
        bindkey "\e[5~"   beginning-of-history # PageUp
        bindkey "\e[6~"   end-of-history       # PageDown
        bindkey "\e[2~"   quoted-insert        # Ins
        bindkey "\e[3~"   delete-char          # Del
        bindkey "\e[1;5C" emacs-forward-word   # Ctrl-Right
        bindkey "\e[1;5D" emacs-backward-word  # Ctrl-Left
        bindkey "\e[1;9C" forward-word         # Alt-Right
        bindkey "\e[1;9D" backward-word        # Alt-Left
        bindkey "\C-w"    kill-region          # Ctrl-w
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
