-----------------------------------------------------------
-- File: AwesomeWM configuration file                    --
-- Author: Nicolas G. Querol <nicolas.gquerol@gmail.com> --
-----------------------------------------------------------

require("awful")
require("awful.autofocus")
require("awful.rules")
require("wibox")
require("beautiful")
require("naughty")
require("vicious")

-- Variable definitions {{{
beautiful.init("/home/nico/.config/awesome/theme.lua")

local home       = os.getenv("HOME")
local terminal   = "urxvtc"
local editor     = "vim"
local editor_cmd = terminal .. " -e " .. editor
local modkey     = "Mod4"

layouts = {
    awful.layout.suit.floating,
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    awful.layout.suit.fair.horizontal,
}
-- }}}

-- Tags {{{
tags = {}
for s = 1, screen.count() do
    tags[s] = awful.tag({ "main", "web", "dev" }, s, layouts[1])
end
-- }}}

-- Menu {{{
myawesomemenu = {
    { "Edit rc.lua", editor_cmd .. "  " ..  awesome.conffile },
    { "Edit theme.lua", editor_cmd .. "  " .. theme.confdir .. "/theme.lua" },
    { "", },
    { "Restart", awesome.restart },
    { "Quit", awesome.quit }
}

mylogoutmenu = {
    { "Restart", "sudo reboot" },
    { "Shutdown", "sudo shutdown -h now" },
    { "Sleep", "sudo pm-suspend" },
    theme = { width = 80, height = 15 }
}

mymainmenu = awful.menu({ 
    items = {
        { "Files", "thunar" },
        { "", },
        {"Terminal", terminal },
        { "", },
        { "Firefox", "firefox" },
        { "GIMP", "gimp" },
        { "GMAMEUI", "gmameui" },
        { "GVIM", "gvim" },
        { "MCabber", terminal .. " -e mcabber"},
        { "Transmission", "transmission-gtk" },
        { "Weechat", terminal .. " -e weechat-curses"},
        { "", },
        { "Awesome", myawesomemenu },
        { "", },
        { "Logout", mylogoutmenu }
    }
})
    
mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })
-- }}}

-- Wibox {{{
-- Date and time
dateicon       = wibox.widget.imagebox()
dateicon:set_image(beautiful.widget_date)
datewidget     = wibox.widget.textbox()
vicious.register(datewidget, vicious.widgets.date, "%d/%m/%y,<span weight='bold'> %R</span>", 59)

-- Spacers
spacer1      = wibox.widget.textbox()
spacer1:set_text(" | ")
spacer2      = wibox.widget.textbox()
spacer2:set_text(" ")

-- Left side
lside       = wibox.widget.imagebox()
lside:set_image(beautiful.widget_left)

-- Right side
rside       = wibox.widget.imagebox()
rside:set_image(beautiful.widget_right)

-- Gmail widget & tooltip
gmailicon       = wibox.widget.imagebox()
gmailicon:set_image(beautiful.widget_mail)
gmailwidget     = wibox.widget.textbox()
vicious.register(gmailwidget, vicious.widgets.gmail,
    function (widget, args)
        if args['{count}'] > 0 then
            return "<span color ='#66AABB' weight ='bold'>"..args["{count}"].."</span>"
        else
            return args['{count}']
        end
    end, 67)

gmailicon:buttons(awful.util.table.join(
    awful.button({ }, 1, function () awful.util.spawn("urxvtc -e mutt") end)))

-- Battery percentage & state
baticon       = wibox.widget.imagebox()
baticon:set_image(beautiful.widget_bat)
batwidget     = wibox.widget.textbox()
vicious.register(batwidget, vicious.widgets.bat,
    function (widget, args)
        -- full/charged
        if args[1] == "↯" then
            return "<span color='#9EDB58' weight='bold'>"..args[2].."%</span>"
        else
            -- charging
            if args[1] == "+" then
                return "<span weight='bold'>"..args[1]..args[2].."%</span> ("..args[3]..")"
            else
                -- low battery
                if args[2] <= 10 then
                    return "<span color='#E84F4F' weight='bold'>"..args[2].."%</span>"
                else
                -- discharging
                    return "<span weight='bold'>"..args[2].."%</span>"
                end
            end
        end
    end, 61, "BAT0")
vicious.cache(vicious.widgets.bat)

-- CPU usage
cpuicon       = wibox.widget.imagebox()
cpuicon:set_image(beautiful.widget_cpu)
cpuwidget     = wibox.widget.textbox()
vicious.register(cpuwidget, vicious.widgets.cpu, "<span weight='bold'>$1%</span>", 2)
vicious.cache(vicious.widgets.cpu)

-- CPU temperature
thermalwidget = wibox.widget.textbox()
vicious.register(thermalwidget, vicious.widgets.thermal,
    function (widget, args)
        if args[1] >= 70 then
            return " @ <span weight='bold' color='#E84F4F'>"..args[1].."°C</span></span>"
        else
            return " @ <span weight='bold'>"..args[1].."°C</span>"
        end
    end, 19, {"coretemp.0", "core"})

-- RAM usage
memicon     = wibox.widget.imagebox()
memicon:set_image(beautiful.widget_mem)
memwidget   = wibox.widget.textbox()
vicious.register(memwidget, vicious.widgets.mem, "<span weight='bold'>$2M</span>", 5)

mywibox     = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist   = {}
mytaglist.buttons = awful.util.table.join(
    awful.button({ }, 1, awful.tag.viewonly),
    awful.button({ modkey }, 1, awful.client.movetotag),
    awful.button({ }, 3, awful.tag.viewtoggle),
    awful.button({ modkey }, 3, awful.client.toggletag),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
)

mytasklist = {}
mytasklist.buttons = awful.util.table.join(
    awful.button({ }, 1, function (c)
        if c == client.focus then
            c.minimized = true
        else
            if not c:isvisible() then
                awful.tag.viewonly(c:tags()[1])
            end
        client.focus = c
        c:raise()
        end
    end),
    awful.button({ }, 3, function ()
        if instance then
            instance:hide()
            instance = nil
        else
            instance = awful.menu.clients({ width=250 })
        end
    end),
    awful.button({ }, 4, function ()
        awful.client.focus.byidx(1)
            if client.focus then client.focus:raise() end
    end),
    awful.button({ }, 5, function ()
        awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
    end)
)

for s = 1, screen.count() do
    mypromptbox[s] = awful.widget.prompt()

    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
        awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
        awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
        awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
        awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)
    ))

    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

    mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

    mywibox[s] = awful.wibox({ position = "bottom", height = "18", screen = s })

    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(mylauncher)
    left_layout:add(spacer2)
    left_layout:add(mytaglist[s])
    left_layout:add(spacer2)
    left_layout:add(mylayoutbox[s])
    left_layout:add(spacer2)
    left_layout:add(mypromptbox[s])
    left_layout:add(spacer2)

    local right_layout = wibox.layout.fixed.horizontal()
    right_layout:add(spacer2)
    right_layout:add(lside)
    right_layout:add(spacer2)
    right_layout:add(cpuicon)
    right_layout:add(cpuwidget)
    right_layout:add(thermalwidget)
    right_layout:add(spacer1)
    right_layout:add(memicon)
    right_layout:add(memwidget)
    right_layout:add(spacer1)
    right_layout:add(baticon)
    right_layout:add(batwidget)
    right_layout:add(spacer1)
    right_layout:add(gmailicon)
    right_layout:add(gmailwidget)
    right_layout:add(spacer1)
    right_layout:add(dateicon)
    right_layout:add(datewidget)
    right_layout:add(spacer1)
    if s == 1 then right_layout:add(wibox.widget.systray()) end
    right_layout:add(spacer2)
    right_layout:add(rside)
    
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    layout:set_middle(mytasklist[s])
    layout:set_right(right_layout)

    mywibox[s]:set_widget(layout)
end
-- }}}

-- Mouse bindings {{{
root.buttons(
    awful.button({ }, 3, function () mymainmenu:toggle() end)
)
-- }}}

-- Key bindings {{{
globalkeys = awful.util.table.join(
    awful.key({ }, "XF86AudioMute",        function () awful.util.spawn(home .. "/.bin/dvol -t") end),
    awful.key({ }, "XF86AudioRaiseVolume", function () awful.util.spawn(home .. "/.bin/dvol -i 5") end),
    awful.key({ }, "XF86AudioLowerVolume", function () awful.util.spawn(home .. "/.bin/dvol -d 5") end),
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),

    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),

    awful.key({ modkey,           }, "w", function () mymainmenu:show({keygrabber=true}) end),
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),

    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then client.focus:raise() end
        end),

    awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    awful.key({ modkey, "Control" }, "n", awful.client.restore),

    awful.key({ modkey },            "r",
        function ()
            awful.prompt.run({ prompt = "<span weight='bold'>Run: </span>" },
                mypromptbox[mouse.screen].widget,
                function (cmd) awful.util.spawn(cmd) end,
                awful.completion.shell,
                awful.util.getdir("cache") .. "/history")
        end),

    awful.key({ modkey, "Shift"   }, "r",
        function ()
            awful.prompt.run({ prompt = "<span weight='bold'>Run in terminal: </span>" },
                mypromptbox[mouse.screen].widget,
                function (cmd) awful.util.spawn(terminal .. ' -e "' .. cmd .. '"') end,
                awful.completion.shell,
                awful.util.getdir("cache") .. "/history")
        end)
)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),

    awful.key({ modkey,           }, "n",
        function (c)
            c.minimized = true
        end),

    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)

keynumber = 0

for s = 1, screen.count() do
    keynumber = math.min(9, math.max(#tags[s], keynumber));
end

for i = 1, keynumber do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. i + 9,
            function ()
                local screen = mouse.screen
                if tags[screen][i] then
                    awful.tag.viewonly(tags[screen][i])
                end
            end),

        awful.key({ modkey, "Control" }, "#" .. i + 9,
            function ()
                local screen = mouse.screen
                if tags[screen][i] then
                    awful.tag.viewtoggle(tags[screen][i])
                end
            end),

        awful.key({ modkey, "Shift" }, "#" .. i + 9,
            function ()
                if client.focus and tags[client.focus.screen][i] then
                    awful.client.movetotag(tags[client.focus.screen][i])
                end
            end),

        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
            function ()
                if client.focus and tags[client.focus.screen][i] then
                    awful.client.toggletag(tags[client.focus.screen][i])
                end
            end)
    )
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize)
)

root.keys(globalkeys)
-- }}}

-- Rules {{{
awful.rules.rules = { 
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = true,
                     keys = clientkeys,
                     size_hints_honor = false,
                     buttons = clientbuttons } },
    { rule = { class = "Firefox" },
      properties = { tag = tags[1][2] } }, 
    { rule = { class = "plugin-container" },
      properties = { floating = true } },    
    { rule = { name = "Téléchargements" },
      properties = { floating = "true" } },
}
-- }}}

-- Signals {{{
client.connect_signal("manage", function (c, startup)

    c:connect_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        awful.client.setslave(c)
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
