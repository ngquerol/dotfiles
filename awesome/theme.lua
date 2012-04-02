-------------------------------
--   "Nada" awesomewm theme  --
--    By Nicolas G. Querol   --
-------------------------------

-- {{{ Main
theme = {}
theme.confdir       = awful.util.getdir("config")
theme.wallpaper_cmd = { "" }
-- }}}

-- {{{ Styles
theme.font          = "Terminusmodx 10"

-- {{{ Colors
theme.bg_normal     = "#191919"
theme.bg_focus      = "#ffffff"
theme.bg_urgent     = "#191919"
theme.bg_minimize   = "#191919"
theme.fg_normal     = "#AAAAAA"
theme.fg_focus      = "#ffffff"
theme.fg_urgent     = "#E64444"
theme.fg_minimize   = "#444444"
-- }}}

-- {{{ Borders
theme.border_width  = "1"
theme.border_normal = "#CCCCCC"
--theme.border_focus  = "#66A9BA"
--theme.border_marked = "#91231c"
-- }}}

-- {{{ Taglist and Tasklist
theme.taglist_bg_focus		= "#191919"
theme.taglist_fg_urgent		= "#ffffff"
theme.tasklist_fg_focus		= "#ffffff"
theme.tasklist_bg_urgent	= "#E64444"
theme.tasklist_fg_urgent	= "#ffffff"
theme.tasklist_bg_focus		= "#191919"
-- }}}

-- {{{ Tooltips
theme.tooltip_bg_color     = "#191919"
theme.tooltip_fg_color     = "#AAAAAA"
theme.tooltip_border_color = "#404040"
theme.tooltip_border_width = "1"
--- }}}

-- {{{ Mouse finder
theme.mouse_finder_color = "#CC9393"
-- }}}

-- {{{ Menu
theme.menu_height	= "15"
theme.menu_width        = "100"
theme.menu_fg_focus     = "#ffffff"
theme.menu_bg_focus     = "#303030"
theme.menu_border_width = "0"
-- }}}

-- }}}


-- {{{ Icons
theme.taglist_squares_sel   = theme.confdir.. "/icons/tagsel.png"
theme.taglist_squares_unsel = theme.confdir.. "/icons/tagunsel.png"
-- }}}

-- {{{ Misc icons
theme.awesome_icon        = theme.confdir.. "/icons/arch.png"
theme.menu_submenu_icon   = theme.confdir.. "/icons/submenu.png"
-- }}}

-- {{{ Widget icons
theme.widget_cpu    = theme.confdir .. "/icons/cpu.png"
theme.widget_bat    = theme.confdir .. "/icons/ac.png"
theme.widget_wifi   = theme.confdir .. "/icons/wifi.png"
theme.widget_mail   = theme.confdir .. "/icons/mail.png"
theme.widget_date   = theme.confdir .. "/icons/clock.png"
theme.widget_right  = theme.confdir .. "/icons/rside.png"
theme.widget_left   = theme.confdir .. "/icons/lside.png"
-- }}}


-- {{{ Layout icons
theme.layout_fairh	    = theme.confdir.. "/icons/layouts/fairhw.png"
theme.layout_fairv	    = theme.confdir.. "/icons/layouts/fairvw.png"
theme.layout_floating	= theme.confdir.. "/icons/layouts/floatingw.png"
theme.layout_tilebottom = theme.confdir.. "/icons/layouts/tilebottomw.png"
theme.layout_tileleft   = theme.confdir.. "/icons/layouts/tileleftw.png"
theme.layout_tile	    = theme.confdir.. "/icons/layouts/tilew.png"
theme.layout_tiletop	= theme.confdir.. "/icons/layouts/tiletopw.png"
-- }}}

return theme
