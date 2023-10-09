local wezterm = require 'wezterm'

local config = {}

if wezterm.config_builder then
    config = wezterm.config_builder()
end

config.font_size = 12.5
config.line_height = 1.1

local super = 'CTRL'

if
    wezterm.target_triple == 'x86_64-apple-darwin'
    or wezterm.target_triple == 'aarch64-apple-darwin'
then
    config.font = wezterm.font_with_fallback { 'Iosevka Term', 'SF Mono', 'Menlo' }
    config.freetype_load_target = 'Light'
    config.freetype_load_flags = 'NO_HINTING'

    super = 'CMD'
end

config.keys = {
    {
        key = 'd',
        mods = super,
        action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' },
    },
    {
        key = 'd',
        mods = super .. '|SHIFT',
        action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
    },
    {
        key = 'LeftArrow',
        mods = super,
        action = wezterm.action.ActivateTabRelativeNoWrap(-1),
    },
    {
        key = 'RightArrow',
        mods = super,
        action = wezterm.action.ActivateTabRelativeNoWrap(1),
    },
    {
        key = 'LeftArrow',
        mods = super .. '|SHIFT',
        action = wezterm.action.MoveTabRelative(-1),
    },
    {
        key = 'RightArrow',
        mods = super .. '|SHIFT',
        action = wezterm.action.MoveTabRelative(1),
    }
}

config.mouse_bindings = {
    {
        event = { Up = { streak = 1, button = 'Left' } },
        mods = super,
        action = wezterm.action.OpenLinkAtMouseCursor,
    },
}

config.colors = {
    foreground = '#dfdfdf',
    background = '#2f2f2f',

    cursor_bg = '#c0c5ce',
    cursor_fg = '#2b303b',
    cursor_border = '#c0c5ce',

    selection_fg = 'black',
    selection_bg = '#fffacd',

    split = '#444444',

    ansi = {
        '#3f3f3f',
        '#ffc0b7',
        '#ecfbdc',
        '#fffcd4',
        '#c4e0f0',
        '#edd0dd',
        '#d6f4fe',
        '#e9e9e9',
    },
    brights = {
        '#5f5f5f',
        '#ffb2a6',
        '#e7fad0',
        '#ffd193',
        '#a3d1fc',
        '#edc1fc',
        '#baf0fd',
        '#e9e9e9',
    },
}

config.initial_cols = 100
config.initial_rows = 25
config.use_resize_increments = true
config.window_decorations = 'RESIZE'
config.window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
}

return config
