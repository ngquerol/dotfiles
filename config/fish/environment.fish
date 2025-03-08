## shell environment configuration

status is-login; or return

set -x LANG "fr_FR.UTF-8"
set -x BROWSER open
set -x EDITOR vi
set -x VISUAL "$EDITOR"
set -x PAGER "less -s -M +Gg"

test -d "$HOME/.bin"; and fish_add_path "$HOME/.bin"
test -d "$HOME/.local/bin"; and fish_add_path "$HOME/.local/bin"

if command -q nvim
    set -x EDITOR nvim
    set -x VISUAL nvim
end

set -x VIRTUAL_ENV_DISABLE_PROMPT 1
