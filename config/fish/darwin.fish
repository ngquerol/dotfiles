## darwin-specific shell configuration

# XDG user directories
test -d "$HOME/.config"; and set -x XDG_CONFIG_HOME "$HOME/.config"
test -d "$HOME/.cache"; and set -x XDG_CACHE_HOME "$HOME/.cache"
test -d "$HOME/.local/share"; and set -x XDG_DATA_HOME "$HOME/.local/share"
test -d "$HOME/.local/state"; and set -x XDG_STATE_HOME "$HOME/.local/state"

# homebrew
if command -q brew
    # homebrew(-cask) options
    set -x HOMEBREW_NO_ANALYTICS 1
    set -x HOMEBREW_NO_INSECURE_REDIRECT 1
    set -x HOMEBREW_NO_ENV_HINTS 1
    set -x HOMEBREW_CASK_OPTS --require-sha
    set -x HOMEBREW_PREFIX (brew --prefix)

    # headers & libraries
    # note: C_INCLUDE_PATH, CPLUS_INCLUDE_PATH, OBJC_INCLUDE_PATH and
    # OBJCPLUS_INCLUDE_PATH are also available for language-specific headers.
    set -x CPATH "$HOMEBREW_PREFIX/include:$CPATH"
    set -x LIBRARY_PATH "$HOMEBREW_PREFIX/lib:$LIBRARY_PATH"

    # binaries & info pages
    fish_add_path "$HOMEBREW_PREFIX/sbin"

    # keg-only LLVM
    fish_add_path "$HOMEBREW_PREFIX/opt/llvm/bin"
end

status is-interactive; or return

# homebrew aliases
if command -q brew
    alias bup "brew upgrade && brew cleanup -s"
end

# silence 'last login' text
test -f ~/.hushlogin; or touch ~/.hushlogin

# darwin-specific abbreviations
abbr top "top -o cpu"
abbr ll "ls -lhFGT"

# delete local Time Machine snapshots
function tmcleanlocal
    set -l snapshots (tmutil listlocalsnapshotdates / | tail +2)

    if test (count $snapshots) -eq 0
        echo "No local Time Machine snapshots found."
        return
    end

    printf "Found %d local snapshot(s): \n\n%s\n\n" \
        (count $snapshots) \
        (string join \n $snapshots | string collect)

    while read --local response --nchars 1 --prompt-str="Proceed with deletion? (y/n) "
        or return 1
        switch $response
            case y Y
                break
            case n N
                return
            case '*'
                echo Invalid input.
                continue
        end
    end

    for s in $snapshots
        printf "Deleting local Time Machine snapshot \"%s\"...\n" $s
        tmutil deletelocalsnapshots $s 1>/dev/null
    end
end
