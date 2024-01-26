## shell environment configuration

status is-login; or return

set -x LANG "fr_FR.UTF-8"
set -x BROWSER "open"
set -x EDITOR "vi"
set -x VISUAL "$EDITOR"
set -x PAGER "less -s -M +Gg"

for path in \
    "$HOME/.bin" \
    "$HOME/.cargo/bin" \
    "$HOME/.orbstack/bin" \
    "/usr/local/opt/binutils/bin"
    fish_add_path $path
end

if command -q go
    set -x GOPATH "$HOME/Developer/go/gopath"
    test -d $GOPATH; and fish_add_path "$GOPATH/bin"
end

if command -q nvim
    set -x EDITOR "nvim"
    set -x VISUAL "nvim"
end

set -x VIRTUAL_ENV_DISABLE_PROMPT 1
