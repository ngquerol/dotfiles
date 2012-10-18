-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
-- Widget library
local vicious = require("vicious")
-- Menu library
local menubar = require("menubar")

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init("/home/nico/.config/awesome/theme.lua")

-- This is used later as the default terminal and editor to run.

os.setlocale(os.getenv("LANG"))

home = os.getenv("HOME")
browser = os.getenv("BROWSER") or "firefox"
editor = os.getenv("EDITOR") or "vim"
terminal = "urxvtc"
editor_cmd = terminal .. " -e " .. editor
fm = "thunar"

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
local layouts =
{
    awful.layout.suit.floating,
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    awful.layout.suit.fair.horizontal,
    awful.layout.suit.spiral,
    awful.layout.suit.spiral.dwindle,
}
-- }}}

-- {{{ Wallpaper
if beautiful.wallpaper then
    for s = 1, screen.count() do
        gears.wallpaper.maximized(beautiful.wallpaper, s, true)
    end
end
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {
    names = { "main", "web", "dev" },
    layout = { layouts[1], layouts[2], layouts[1] },
    icons = { "/home/nico/.config/awesome/icons/taglist/main.png", "/home/nico/.config/awesome/icons/taglist/web.png", "/home/nico/.config/awesome/icons/taglist/dev.png" }
}

for s = 1, screen.count() do
    tags[s] = awful.tag(tags.names, s, tags.layout)

    for i, t in pairs(tags[s]) do
        awful.tag.seticon(tags.icons[i], t)
    end
end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
myawesomemenu = {
    { "Éditer rc.lua", editor_cmd .. " " .. awesome.conffile },
    { "Éditer theme.lua", editor_cmd .. " " .. theme.confdir .. "/theme.lua" },
    { "", },
    { "Redémarrer", awesome.restart },
    { "Quitter", awesome.quit },
    theme = { width = 125, height = 15 }
}

mylogoutmenu = {
    { "Redémarrer", "systemctl reboot" },
    { "Éteindre", "systemctl poweroff" },
    { "Mettre en veille", "systemctl suspend" },
    theme = { width = 125, height = 15 }
}

myfilesmenu = {
    { "Code", fm .. " " .. home .. "/Code" },
    { "Documents", fm .. " " .. home .. "/Documents" },
    { "Dropbox", fm .. " " .. home .. "/Dropbox" },
    { "Git", fm .. " " .. home .. "/Git" },
    { "Images", fm .. " " .. home .. "/Images" },
    { "Téléchargements", fm .. " " .. home .. "/Téléchargements" },
    theme = { width = 125, height = 15 }
}

mymainmenu = awful.menu({
    items = {
        { "Fichiers", myfilesmenu },
        { "", },
        {"Terminal", terminal },
        { "", },
        { "Firefox", "firefox" },
        { "GIMP", "gimp" },
        { "GVIM", "gvim" },
        { "Ncmpcpp", terminal .. " -e ncmpcpp" },
        { "Transmission", "transmission-gtk" },
        { "Weechat", terminal .. " -e weechat-curses" },
        { "", },
        { "Awesome", myawesomemenu },
        { "", },
        { "Déconnexion", mylogoutmenu }
    }
})

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu  = mymainmenu })
-- }}}

-- {{{ Wibox
-- Spacers
spacer1 = wibox.widget.imagebox()
spacer1:set_image(beautiful.widget_spacer)

-- Left side
lside = wibox.widget.imagebox()
lside:set_image(beautiful.widget_left)

-- Right side
rside = wibox.widget.imagebox()
rside:set_image(beautiful.widget_right)

-- Date and time
dateicon  = wibox.widget.imagebox()
dateicon:set_image(beautiful.widget_date)
datewidget = wibox.widget.textbox()
vicious.register(datewidget, vicious.widgets.date, "%d/%m/%y, <b>%R</b>", 59)

-- Gmail widget
gmailicon       = wibox.widget.imagebox()
gmailicon:set_image(beautiful.widget_mail)
gmailwidget     = wibox.widget.textbox()
vicious.register(gmailwidget, vicious.widgets.gmail,
function (widget, args)
    if args['{count}'] > 1 then
        return "<span color ='#66AABB'><b>" .. args["{count}"] .. "</b></span>"
    elseif args['{count}'] == 1 then
        return "<span color ='#66AABB'><b>" .. args["{count}"] .. "</b></span>"
    else
        return args['{count}']
    end
end, 67)
vicious.cache(vicious.widgets.gmail)

gmailicon:buttons(awful.util.table.join(
awful.button({ }, 1, function () awful.util.spawn(browser .. " https://mail.google.com/mail/") end)))

-- Battery percentage and state
baticon = wibox.widget.imagebox()
baticon:set_image(beautiful.widget_bat)
batwidget     = wibox.widget.textbox()
vicious.register(batwidget, vicious.widgets.bat,
function (widget, args)
    -- full/charged
    if args[1] == "↯" then
        return "<span color='#9EDB58'><b>100%</b></span>"
    -- charging
    elseif args[1] == "+" then
        return "<span weight='bold'>" .. args[1] .. args[2] .. "%</span>"
    -- low battery
    elseif args[2] <= 10 then
        return "<span color='#E84F4F' weight='bold'>" .. args[2] .. "%</span>"
    else
        -- discharging
        return "<span weight='bold'>" .. args[2] .. "%</span>"
    end
end, 61, "BAT0")
vicious.cache(vicious.widgets.bat)

-- CPU usage
cpuicon       = wibox.widget.imagebox()
cpuicon:set_image(beautiful.widget_cpu)
cpuwidget     = wibox.widget.textbox()
vicious.register(cpuwidget, vicious.widgets.cpu,
function (widget, args)
    if args[1] >= 75 then
        return "<span weight='bold' color='#E84F4F'>" .. args[1] .. "%</span>"
    else
        return "<b>".. args[1] .. "%</b>"
    end
end, 3)
vicious.cache(vicious.widgets.cpu)

-- CPU temperature
thermalwidget = wibox.widget.textbox()
vicious.register(thermalwidget, vicious.widgets.thermal,
function (widget, args)
    if args[1] >= 70 then
        return " @ <span color='#E84F4F'><b>" .. args[1] .. "Â°C</b></span>"
    else
        return " @ <b>" .. args[1] .. "°C</b>"
    end
end, 19, {"coretemp.0", "core"})

-- RAM usage
memicon     = wibox.widget.imagebox()
memicon:set_image(beautiful.widget_mem)
memwidget   = wibox.widget.textbox()
vicious.register(memwidget, vicious.widgets.mem,
function (widget, args)
    if args[1] >= 75 then
        return "<span color='#E84F4F'><b>" .. args[2] .. "M</b></span>"
    else
        return "<b>" .. args[2] .. "M</b>"
    end
end, 5)
vicious.cache(vicious.widgets.mem)

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() then
                                                      awful.tag.viewonly(c:tags()[1])
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
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
                                          end))

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "bottom", height = "18", screen = s })

    -- Widgets that are aligned to the left
    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(lside)
    left_layout:add(mytaglist[s])
    left_layout:add(spacer1)
    left_layout:add(mylayoutbox[s])
    left_layout:add(rside)
    left_layout:add(mypromptbox[s])

    -- Widgets that are aligned to the right
    local right_layout = wibox.layout.fixed.horizontal()
    right_layout:add(lside)
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
    if s == 1 then
    right_layout:add(spacer1)
    right_layout:add(wibox.widget.systray())
    right_layout:add(rside)
    else
    right_layout:add(rside)
    end

    -- Now bring it all together (with the tasklist in the middle)
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    layout:set_right(right_layout)

    mywibox[s]:set_widget(layout)
end
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
awful.key({  }, "XF86AudioMute",        function () awful.util.spawn_with_shell(home .. "/.bin/dvol -t") end),
awful.key({  }, "XF86AudioRaiseVolume", function () awful.util.spawn_with_shell(home .. "/.bin/dvol -i 5") end),
awful.key({  }, "XF86AudioLowerVolume", function () awful.util.spawn_with_shell(home .. "/.bin/dvol -d 5") end),
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
    awful.key({ modkey,           }, "w", function () mymainmenu:show() end),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    -- Standard program
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

    -- Prompt
    awful.key({ modkey  },            "r",
    function ()
        awful.prompt.run({ prompt = "<span weight='bold'> Exécuter : </span>"  },
        mypromptbox[mouse.screen].widget,
        function (cmd) awful.util.spawn(cmd) end,
        awful.completion.shell,
        awful.util.getdir("cache") .. "/history")
    end),

    awful.key({ modkey, "Shift"    }, "r",
    function ()
        awful.prompt.run({ prompt = "<span weight='bold'> Exécuter dans un terminal : </span>"  },
        mypromptbox[mouse.screen].widget,
        function (cmd) awful.util.spawn(terminal .. " -e " .. cmd) end,
        awful.completion.shell,
        awful.util.getdir("cache") .. "/history")
    end ),

    -- Menubar
    awful.key({ modkey, }, "p", function () menubar.show() end)
)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber))
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
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
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     size_hints_honor = false } },

    { rule = { class = "Firefox" },
      properties = { tag = tags[1][2] } },
    { rule = { class = "Download" },
      properties = { floating = true } },
    { rule = { name = "plugin-container" },
      properties = { floating = true } }
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
    -- Enable sloppy focus
    c:connect_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
