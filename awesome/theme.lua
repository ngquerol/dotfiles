--
-- AwesomeWM "Nada" theme
--
-- Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>
--
-- {{{ Main
theme = {}
theme.confdir   = "~/.config/awesome/"
theme.wallpaper = "~/Dropbox/wallpapers/dlanham-Kodama.jpg"
-- }}}

-- {{{ Styles

theme.font          = "Terminusmodx 9"
theme.icon_theme    = "Faenza"

-- {{{ Colors
theme.bg_normal         = "#3F3F3F"
theme.bg_focus          = "#1E2320"
theme.bg_urgent         = "#3F3F3F"
theme.bg_minimize       = "#191919"
theme.bg_systray        = theme.bg_normal
theme.fg_normal         = "#DCDCCC"
theme.fg_focus          = "#F0DFAF"
theme.fg_urgent         = "#CC9393"
theme.fg_minimize       = "#444444"
-- }}}

-- {{{ Borders
theme.border_width  = "1"
theme.border_normal = "#3F3F3F"
theme.border_focus  = "#CC9393"
theme.border_marked = theme.border_focus
-- }}}

-- {{{ Taglist
theme.taglist_bg_focus      = "#1E2320"
theme.taglist_fg_focus      = "#00CCFF"
theme.taglist_fg_urgent     = "#00CCFF"
-- }}}

-- {{{ Titlebar
theme.titlebar_bg_focus    = "#3F3F3F"
theme.titlebar_bg_normal   = "#3F3F3F"
-- }}}

-- {{{ Tooltips
theme.tooltip_bg_color        = theme.bg_normal
theme.tooltip_fg_color        = theme.fg_normal
theme.tooltip_border_color    = theme.border_focus
theme.tooltip_fg_color_urgent = "#E84F4F"
theme.tooltip_border_width    = "1"
-- }}}

-- {{{ Menu
theme.menu_height       = "16"
theme.menu_width        = "110"
theme.menu_fg_focus     = "#ffffff"
theme.menu_bg_focus     = "#303030"
theme.menu_border_color = "#404040"
theme.menu_border_width = "1"
-- }}}

-- {{{ Notifications
theme.notify_fg     = theme.fg_normal
theme.notify_bg     = theme.bg_normal
theme.notify_border = theme.border_focus
-- }}}

-- }}}

-- {{{ Icons

-- Taglist {{{
theme.taglist_squares_sel   = theme.confdir .. "/icons/taglist/square_sel.png"
theme.taglist_squares_unsel = theme.confdir .. "/icons/taglist/square_unsel.png"
-- }}}

-- {{{ Misc icons
theme.menu_submenu_icon     = theme.confdir .. "/icons/submenu.png"
-- }}}

-- {{{ Widget icons
theme.widget_cpu        = theme.confdir .. "/icons/widgets/cpu.png"
theme.widget_cpu_clear  = theme.confdir .. "/icons/widgets/cpu_clear.png"
theme.widget_mem        = theme.confdir .. "/icons/widgets/mem.png"
theme.widget_bat        = theme.confdir .. "/icons/widgets/battery.png"
theme.widget_mail       = theme.confdir .. "/icons/widgets/mail.png"
theme.widget_mpd        = theme.confdir .. "/icons/widgets/music.png"
theme.widget_date       = theme.confdir .. "/icons/widgets/cal.png"
theme.widget_wifi       = theme.confdir .. "/icons/widgets/net.png"
theme.widget_hdd        = theme.confdir .. "/icons/widgets/hdd.png"
theme.widget_hdd_clear  = theme.confdir .. "/icons/widgets/hdd_clear.png"
theme.widget_temp       = theme.confdir .. "/icons/widgets/temp.png"
theme.arrl              = theme.confdir .. "/icons/widgets/arrl.png"
theme.arrl_dl           = theme.confdir .. "/icons/widgets/arrl_dl.png"
theme.arrl_dl_sf        = theme.confdir .. "/icons/widgets/arrl_dl_sf.png"
theme.arrl_ld           = theme.confdir .. "/icons/widgets/arrl_ld.png"
theme.arrl_ld_sf        = theme.confdir .. "/icons/widgets/arrl_ld_sf.png"
theme.arrl_sf           = theme.confdir .. "/icons/widgets/arrl_sf.png"
-- }}}

-- {{{ Layout icons
theme.layout_floating   = theme.confdir .. "/icons/layouts/floating.png"
theme.layout_tilebottom = theme.confdir .. "/icons/layouts/tilebottom.png"
theme.layout_tileleft   = theme.confdir .. "/icons/layouts/tileleft.png"
theme.layout_tile       = theme.confdir .. "/icons/layouts/tile.png"
theme.layout_tiletop    = theme.confdir .. "/icons/layouts/tiletop.png"
-- }}}

-- {{{ Titlebar
theme.titlebar_close_button_focus               = theme.confdir .. "/icons/titlebar/close_focus.png"
theme.titlebar_close_button_normal              = theme.confdir .. "/icons/titlebar/close_normal.png"
theme.titlebar_ontop_button_focus_active        = theme.confdir .. "/icons/titlebar/ontop_focus_active.png"
theme.titlebar_ontop_button_normal_active       = theme.confdir .. "/icons/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_inactive      = theme.confdir .. "/icons/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_inactive     = theme.confdir .. "/icons/titlebar/ontop_normal_inactive.png"
theme.titlebar_sticky_button_focus_active       = theme.confdir .. "/icons/titlebar/sticky_focus_active.png"
theme.titlebar_sticky_button_normal_active      = theme.confdir .. "/icons/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_inactive     = theme.confdir .. "/icons/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_inactive    = theme.confdir .. "/icons/titlebar/sticky_normal_inactive.png"
theme.titlebar_floating_button_focus_active     = theme.confdir .. "/icons/titlebar/floating_focus_active.png"
theme.titlebar_floating_button_normal_active    = theme.confdir .. "/icons/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_inactive   = theme.confdir .. "/icons/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_inactive  = theme.confdir .. "/icons/titlebar/floating_normal_inactive.png"
theme.titlebar_maximized_button_focus_active    = theme.confdir .. "/icons/titlebar/maximized_focus_active.png"
theme.titlebar_maximized_button_normal_active   = theme.confdir .. "/icons/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_inactive  = theme.confdir .. "/icons/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_inactive = theme.confdir .. "/icons/titlebar/maximized_normal_inactive.png"
-- }}}

return theme
