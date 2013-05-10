# Emacs-style kybindings (doh!)
bindkey -e

# Different keybindings for different terminals...
case $TERM in
    rxvt*)
        bindkey "\e[7~"  beginning-of-line     # Home
        bindkey "\e[8~"  end-of-line           # End
        bindkey "\e[5~"  beginning-of-history  # PageUp
        bindkey "\e[6~"  end-of-history        # PageDown
        bindkey "\e[2~"  quoted-insert         # Ins
        bindkey "\e[3~"  delete-char           # Del
        bindkey "\eOc"   emacs-forward-word    # Ctrl-Right
        bindkey "\eOd"   emacs-backward-word   # Ctrl-Left
        bindkey "\e\e[C" forward-word          # Alt-Right
        bindkey "\e\e[D" backward-word         # Alt-Left
        ;;
    xterm*)
        bindkey "\eOH"   beginning-of-line     # Home
        bindkey "\eOF"   end-of-line           # End
        bindkey "\e[5~"   beginning-of-history # PageUp
        bindkey "\e[6~"   end-of-history       # PageDown
        bindkey "\e[2~"   quoted-insert        # Ins
        bindkey "\e[3~"   delete-char          # Del
        bindkey "\e[1;5C" emacs-forward-word   # Ctrl-Right
        bindkey "\e[1;5D" emacs-backward-word  # Ctrl-Left
        bindkey "\e[1;3C" forward-word         # Alt-Right
        bindkey "\e[1;3D" backward-word        # Alt-Left
        ;;
    linux*)
        bindkey "\e[1~" beginning-of-line      # Home
        bindkey "\e[4~" end-of-line            # End
        bindkey "\e[5~" beginning-of-history   # PageUp
        bindkey "\e[6~" end-of-history         # PageDown
        bindkey "\e[2~" quoted-insert          # Ins
        bindkey "\e[3~" delete-char            # Del
esac
