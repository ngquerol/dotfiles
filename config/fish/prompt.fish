## prompt configuration

status is-interactive; or return

function fish_prompt
    set -l last_status $status

    # user & hostname, if in a SSH session
    if test -n "$SSH_CLIENT" -o -n "$SSH_TTY"
        set_color bryellow -o
        printf "%s@%s ⚡️ " (whoami) (prompt_hostname)
        set_color normal
    end

    # working directory
    set_color brgreen -o
    echo -n (prompt_pwd)
    set_color normal

    # elapsed time during last command
    if test "$CMD_DURATION" -ge 500
        set -l minutes (math -s0 $CMD_DURATION / 60000)
        set -l seconds (math -s0 $CMD_DURATION % 60000 / 1000)
        set -l milliseconds (math -s0 $CMD_DURATION % 1000)
        set_color brblack
        echo -ns " " $minutes m $seconds s $milliseconds ms
        set_color normal
    end

    # last exit status, if > 0
    if test $last_status -ne 0
        set_color brred -o
        echo -n " $last_status"
    else
        set_color normal
    end

    # prompt symbol
    echo -n " → "
    set_color normal
end

function fish_right_prompt
    # background jobs
    if test (jobs | wc -l) -gt 0
        set_color brblack -o
        echo -ns " " (jobs | wc -l) j
        set_color normal
    end

    # current python virtualenv
    if test -n "$VIRTUAL_ENV"
        echo -ns " 🐍 " (path basename $VIRTUAL_ENV) " (" (python3 --version | string trim -c  'Python ') ")"
    end

    # git info
    echo -n " $(_git_prompt)"
end

# print newline between prompts, if anything was output
function postexec_test --on-event fish_postexec
    echo
end

# mostly stolen from https://github.com/IlanCosman/tide/blob/main/functions/_tide_item_git.fish
function _git_prompt
    set -l loc_len 30

    if git branch --show-current 2>/dev/null | string shorten -m$loc_len | read -l location
        git rev-parse --git-dir --is-inside-git-dir | read -fL gdir in_gdir
        set location $location
    else if test $pipestatus[1] != 0
        return
    else if git tag --points-at HEAD | string shorten -m$loc_len | read location
        git rev-parse --git-dir --is-inside-git-dir | read -fL gdir in_gdir
        set location "#"$location
    else
        git rev-parse --git-dir --is-inside-git-dir --short HEAD | read -fL gdir in_gdir location
        set location @$location
    end

    if test -d $gdir/rebase-merge
        if not path is -v $gdir/rebase-merge/{msgnum,end}
            read -f step <$gdir/rebase-merge/msgnum
            read -f total_steps <$gdir/rebase-merge/end
        end
        test -f $gdir/rebase-merge/interactive && set -f operation rebase-i || set -f operation rebase-m
    else if test -d $gdir/rebase-apply
        if not path is -v $gdir/rebase-apply/{next,last}
            read -f step <$gdir/rebase-apply/next
            read -f total_steps <$gdir/rebase-apply/last
        end
        if test -f $gdir/rebase-apply/rebasing
            set -f operation rebase
        else if test -f $gdir/rebase-apply/applying
            set -f operation am
        else
            set -f operation am/rebase
        end
    else if test -f $gdir/MERGE_HEAD
        set -f operation merge
    else if test -f $gdir/CHERRY_PICK_HEAD
        set -f operation cherry-pick
    else if test -f $gdir/REVERT_HEAD
        set -f operation revert
    else if test -f $gdir/BISECT_LOG
        set -f operation bisect
    end

    test $in_gdir = true && set -l _set_dir_opt -C $gdir/..
    set -l stat (git $_set_dir_opt --no-optional-locks status --porcelain 2>/dev/null)
    string match -qr \
        "(0|(?<stash>.*))\n(0|(?<conflicted>.*))\n(0|(?<staged>.*))\n(0|(?<dirty>.*))\n(0|(?<untracked>.*))(\n(0|(?<behind>.*))\t(0|(?<ahead>.*)))?" \
        "$(
            git stash list 2>/dev/null | count
            string match -r ^UU $stat | count
            string match -r ^[ADMR] $stat | count
            string match -r ^.[ADMR] $stat | count
            string match -r '^\?\?' $stat | count
            git rev-list --count --left-right @{upstream}...HEAD 2>/dev/null
          )"

    echo -n (
        set_color blue; echo -ns "± $location"
        set_color magenta; echo -ns " "$operation " "$step/$total_steps
        set_color red; echo -ns " ↓"$behind
        set_color green; echo -ns " ↑"$ahead
        set_color purple; echo -ns " \$"$stash
        set_color red; echo -ns " ~"$conflicted
        set_color green; echo -ns " +"$staged
        set_color cyan; echo -ns " !"$dirty
        set_color yellow; echo -ns " ?"$untracked
    )
end
