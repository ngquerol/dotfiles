## machine-local shell environment configuration

status is-login; or return

if command -q go
    set -x GOPATH "$HOME/Developer/go"
    fish_add_path "$GOPATH/bin"
end

if test -d "$HOME/.cargo"
    fish_add_path "$HOME/.cargo/bin"
end

if test -d "$HOME/.orbstack/bin"
    fish_add_path "$HOME/.orbstack/bin"
end
