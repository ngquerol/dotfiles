## Title & git stuffs
precmd() {

    setprompt

    case $TERM in
	xterm*)
	    print -Pn "\e]0;%n@%m : %~\a" ;;
    esac

    vcs_info
}

preexec() {
     case $TERM in
 	xterm*)
 	    print -Pn "\e]0;$1\a" ;;
     esac
}

## Compress/decompress various archive types
unpack() {

    if [[ $# -lt 1 ]]; then
	echo "Veuillez spécifier au moins une archive à décompresser."
    fi
    for a in $@; do
	case $a in
	    *.tar.bz2) tar xvjf $a ;;
	    *.tar.gz) tar xvzf $a ;;
	    *.bz2) bunzip2 $a ;;
	    *.rar) unrar x $a ;;
	    *.gz) gunzip $a ;;
	    *.tar) tar xvf $a ;;
	    *.tbz2) tar xvjf $a ;;
	    *.tgz) tar xvzf $a ;;
	    *.zip) unzip $a ;;
	    *.Z) uncompress $a ;;
	    *.7z) 7z x $a ;;
	    *.xz) xz -d $a ;;
	    *.tar.xz) tar -xvJf $a ;;
	    *) echo "Erreur: l'archive '$a' n'a pas pu être décompressée." ;;
	esac
    done
}

pack() {

    if [[ $# -lt 2 ]]; then
	echo "Compresse fichiers et répertoires via:"
	echo "  pack archive_file file [dir|file]*"
	return 1
    fi

    [[ -f $1 ]] && echo "Erreur: le fichier de destination existe déjà." && return 1

    local lower
    lower=${(L)1}
    case $lower in
	*.tar.bz2) tar cvjf $@ ;;
	*.tar.gz) tar cvzf $@ ;;
	*.tar.xz) tar cvJf $@ ;;
	*.tar.lzma) tar --lzma -cvf $@ ;;
	*.gz) gzip -9r $@ ;;
	*.tar) tar cvf $@ ;;
	*.bz2) bzip2 -9z $@ ;;
	*.xz) xz -7ze $@ ;;
	*.rar) rar a -m5 -r $@; rar k $1 ;;
	*.zip) zip -9r $@ ;;
	*.7z) 7z a -mx9  -t7z -mmt $@ ;;
	*) echo "Erreur: type de compression inconnu ou non renseigné." ;;
    esac
}

## Coloring man pages
man() {

    env \
	LESS_TERMCAP_mb=$(printf "\e[1;34m") \
	LESS_TERMCAP_md=$(printf "\e[1;32m") \
	LESS_TERMCAP_me=$(printf "\e[0m") \
	LESS_TERMCAP_se=$(printf "\e[0m") \
	LESS_TERMCAP_so=$(printf "\e[1;47;30m") \
	LESS_TERMCAP_ue=$(printf "\e[0m") \
	LESS_TERMCAP_us=$(printf "\e[0;32m") \
	man "$@"
}
