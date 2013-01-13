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

--- Variable definitions

-- This is used later as the default terminal and editor to run.
home = os.getenv("HOME")
browser = os.getenv("BROWSER") or "firefox"
editor = os.getenv("EDITOR") or "emacsclient -t"
terminal = "urxvtc"
editor_cmd = terminal .. " -e " .. editor
fm = "thunar"

-- Language (dates for instance)
os.setlocale(os.getenv("LANG"))

-- Themes define colours, icons, and wallpapers
beautiful.init( home .. "/.config/awesome/theme.lua")

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
   }

   --- Wallpaper
   if beautiful.wallpaper then
      for s = 1, screen.count() do
         gears.wallpaper.maximized(beautiful.wallpaper, s, false)
      end
   end

   --- Tags
   -- Define a tag table which hold all screen tags.
   tags = {}
   for s = 1, screen.count() do
      tags[s] = awful.tag({ "term", "web", "misc" }, s, { layouts[1], layouts[2], layouts[1] })
   end

   --- Menu
   -- Create a laucher widget and a main menu
   myawesomemenu = {
      { "Éditer rc.lua", editor_cmd .. " " .. awesome.conffile },
      { "Éditer theme.lua", editor_cmd .. " " .. theme.confdir .. "/theme.lua" },
      { "", },
      { "Redémarrer", awesome.restart },
      { "Quitter", awesome.quit },
   }

   mylogoutmenu = {
      { "Redémarrer", "systemctl reboot" },
      { "Éteindre", "systemctl poweroff" },
      { "Mettre en veille", "systemctl suspend" },
   }

   myfilesmenu = {
      { "Code", fm .. " " .. home .. "/Code" },
      { "Documents", fm .. " " .. home .. "/Documents" },
      { "Dropbox", fm .. " " .. home .. "/Dropbox" },
      { "Git", fm .. " " .. home .. "/Git" },
      { "Images", fm .. " " .. home .. "/Images" },
      { "Téléchargements", fm .. " " .. home .. "/Téléchargements" },
   }

   mymainmenu = awful.menu({
                              items = {
                                 { "Fichiers", myfilesmenu },
                                 { "", },
                                 {"Terminal", terminal },
                                 { "", },
                                 { "GNU Emacs", "emacsclient -c" },
                                 { "Firefox", "firefox" },
                                 { "GIMP", "gimp" },
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

   --- Wibox
   -- Spacers
   spr = wibox.widget.textbox()
   spr:set_text(' ')
   sprd = wibox.widget.textbox()
   sprd:set_markup('<span background="#313131"> </span>')
   spr3f = wibox.widget.textbox()
   spr3f:set_markup('<span background="#777e76"> </span>')
   arrl = wibox.widget.imagebox()
   arrl:set_image(beautiful.arrl)
   arrl_dl = wibox.widget.imagebox()
   arrl_dl:set_image(beautiful.arrl_dl)
   arrl_dl_sf = wibox.widget.imagebox()
   arrl_dl_sf:set_image(beautiful.arrl_dl_sf)
   arrl_ld = wibox.widget.imagebox()
   arrl_ld:set_image(beautiful.arrl_ld)
   arrl_ld_sf = wibox.widget.imagebox()
   arrl_ld_sf:set_image(beautiful.arrl_ld_sf)
   arrl_sf = wibox.widget.imagebox()
   arrl_sf:set_image(beautiful.arrl_sf)

   -- Date and time
   dateicon  = wibox.widget.imagebox()
   dateicon:set_image(beautiful.widget_date)
   local datewidget = wibox.widget.textbox()
   vicious.register(datewidget, vicious.widgets.date, " %H:%M, %a %d/%m/%y ", 59)

   -- Battery percentage and state
   baticon = wibox.widget.imagebox()
   baticon:set_image(beautiful.widget_bat)
   batwidget     = wibox.widget.textbox()
   vicious.register(batwidget, vicious.widgets.bat,
                    function (widget, args)
                       -- full/charged
                       if args[1] == "↯" then
                          return "<span color='#7AC82E'> 100% </span>"
                          -- charging
                       elseif args[1] == "+" then
                          return (" %s%i%% "):format(args[1],args[2])
                          -- low battery
                       elseif args[2] <= 10 then
                          return ("<span color='#E84F4F' weight='bold'> %i%% </span>"):format(args[2])
                       else
                          -- discharging
                          return (" %i%% "):format(args[2])
                       end
                    end, 61, "BAT0")
   vicious.cache(vicious.widgets.bat)

   -- CPU usage
   cpuicon       = wibox.widget.imagebox()
   cpuicon:set_image(beautiful.widget_cpu)
   local cpuwidget      = wibox.widget.background()
   local cpuwidget_text = wibox.widget.textbox()
   vicious.register(cpuwidget_text, vicious.widgets.cpu,
                    function (widget, args)
                       if args[1] >= 75 then
                          return (" <span weight='bold' color='#E84F4F'>  %i  %</span> · <span weight='bold' color='#E84F4F'>  %i  %</span> "):format(args[2], args[3])
                       else
                          return (" %i%% · %i%% "):format(args[2], args[3])
                       end
                    end, 3)
   cpuwidget:set_widget(cpuwidget_text)
   cpuwidget:set_bg("#313131")
   vicious.cache(vicious.widgets.cpu)

   -- RAM usage
   memicon     = wibox.widget.imagebox()
   memicon:set_image(beautiful.widget_mem)
   local memwidget   = wibox.widget.background()
   local memwidget_text = wibox.widget.textbox()
   vicious.register(memwidget_text, vicious.widgets.mem,
                    function (widget, args)
                       if args[1] >= 75 then
                          return ("<span color='#E84F4F'><b> %.fM</b> </span>"):format(args[2])
                       else
                          return (" %iM "):format(args[2])
                       end
                    end, 5)
   memwidget:set_widget(memwidget_text)
   vicious.cache(vicious.widgets.mem)

   -- MPD currently playing song
   mpdicon     = wibox.widget.imagebox()
   mpdicon:set_image(beautiful.widget_mpd)
   mpdwidget   = wibox.widget.textbox()
   vicious.register(mpdwidget, vicious.widgets.mpd,
                    function (widget, args)
                       if args["{state}"] == "Pause" then
                          return " (en pause) "
                       elseif args["{state}"] == "Stop" then
                          return " (arrêté) "
                       elseif args["{state}"] == "Play" then
                          return (" <b>%s</b> - %s "):format(args["{Title}"], args["{Artist}"])
                       else
                          return " "
                       end
                    end, 7)
   mpdicon:buttons(awful.util.table.join(
                      awful.button({ }, 1, function () awful.util.spawn_with_shell(terminal .. " -e ncmpcpp -s playlist")
                                           end)))

   -- Wifi
   wifiicon = wibox.widget.imagebox()
   wifiicon:set_image(beautiful.widget_wifi)
   local wifiwidget = wibox.widget.background()
   local wifiwidget_text = wibox.widget.textbox()
   local wifitooltip = awful.tooltip({})
   wifitooltip:add_to_object(wifiwidget_text)
   vicious.register(wifiwidget_text, vicious.widgets.wifi, function(widget, args)
                       local tooltip = (" <b>mode</b> %s <b>canal</b> %s <b>débit</b> %s Mb/s "):format(
                          args["{mode}"], args["{chan}"], args["{rate}"])
                       local quality = 0
                       if args["{linp}"] > 0 then
                          quality = args["{link}"] / args["{linp}"] * 100
                       end
                       wifitooltip:set_text(tooltip)
                       return (" %s: %i%% "):format(args["{ssid}"], quality)
                                                           end, 13, "wlan0")
   wifiwidget:set_widget(wifiwidget_text)
   wifiwidget:set_bg("#313131")

   -- FS usage
   fsicon = wibox.widget.imagebox()
   fsicon:set_image(beautiful.widget_hdd)
   local fswidget = wibox.widget.background()
   local fswidget_text = wibox.widget.textbox()
   vicious.register(fswidget_text, vicious.widgets.fs,
                    ' ${/ avail_gb}GB disp. ', 67)
   fswidget:set_widget(fswidget_text)
   fswidget:set_bg("#313131")
   fsicon.visible = false

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
      mywibox[s] = awful.wibox({ position = "top", height = "16", screen = s })

      -- Widgets that are aligned to the left
      local left_layout = wibox.layout.fixed.horizontal()
      left_layout:add(mytaglist[s])
      left_layout:add(mypromptbox[s])

      -- Widgets that are aligned to the right
      local right_layout = wibox.layout.fixed.horizontal()
      right_layout:add(mpdicon)
      right_layout:add(mpdwidget)
      right_layout:add(arrl_ld)
      right_layout:add(cpuicon)
      right_layout:add(cpuwidget)
      right_layout:add(arrl_dl)
      right_layout:add(memicon)
      right_layout:add(memwidget)
      right_layout:add(arrl_ld)
      right_layout:add(fsicon)
      right_layout:add(fswidget)
      right_layout:add(arrl_dl)
      right_layout:add(baticon)
      right_layout:add(batwidget)
      right_layout:add(arrl_ld)
      right_layout:add(wifiicon)
      right_layout:add(wifiwidget)
      right_layout:add(arrl_dl)
      right_layout:add(dateicon)
      right_layout:add(datewidget)
      right_layout:add(arrl)
      right_layout:add(spr)
      right_layout:add(wibox.widget.systray())
      right_layout:add(spr)
      right_layout:add(arrl_ld)
      right_layout:add(mylayoutbox[s])

      -- Now bring it all together (with the tasklist in the middle)
      local layout = wibox.layout.align.horizontal()
      layout:set_left(left_layout)
      layout:set_right(right_layout)

      mywibox[s]:set_widget(layout)
   end

   --- Mouse bindings
   root.buttons(awful.util.table.join(
                   awful.button({ }, 3, function () mymainmenu:toggle()
                                        end)))

   --- Key bindings
   globalkeys = awful.util.table.join(
      awful.key({  }, "XF86AudioMute",        function () awful.util.spawn_with_shell(home .. "/.bin/dvol -t") end),
      awful.key({  }, "XF86AudioRaiseVolume", function () awful.util.spawn_with_shell(home .. "/.bin/dvol -i 5") end),
      awful.key({  }, "XF86AudioLowerVolume", function () awful.util.spawn_with_shell(home .. "/.bin/dvol -d 5") end),
      awful.key({  }, "XF86AudioPlay",        function () awful.util.spawn_with_shell("ncmpcpp toggle") end),
      awful.key({  }, "XF86AudioStop",        function () awful.util.spawn_with_shell("ncmpcpp stop") end),
      awful.key({  }, "XF86AudioNext",        function () awful.util.spawn_with_shell("ncmpcpp next") end),
      awful.key({  }, "XF86AudioPrev",        function () awful.util.spawn_with_shell("ncmpcpp prev") end),
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
                   awful.prompt.run({ prompt = " Exécuter : "  },
                                    mypromptbox[mouse.screen].widget,
                                    function (cmd) awful.util.spawn(cmd) end,
                                    awful.completion.shell,
                                    awful.util.getdir("cache") .. "/history")
                end),

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

   --- Rules
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
      { rule = { name = "Téléchargements" },
        properties = { floating = true } },
      { rule = { name = "plugin-container" },
        properties = { floating = true } }
   }

   --- Signals
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

                            local titlebars_enabled = false
                            if titlebars_enabled and (c.type == "normal" or c.type == "dialog") then
                               -- Widgets that are aligned to the left
                               local left_layout = wibox.layout.fixed.horizontal()
                               left_layout:add(awful.titlebar.widget.iconwidget(c))

                               -- Widgets that are aligned to the right
                               local right_layout = wibox.layout.fixed.horizontal()
                               right_layout:add(awful.titlebar.widget.floatingbutton(c))
                               right_layout:add(awful.titlebar.widget.maximizedbutton(c))
                               right_layout:add(awful.titlebar.widget.stickybutton(c))
                               right_layout:add(awful.titlebar.widget.ontopbutton(c))
                               right_layout:add(awful.titlebar.widget.closebutton(c))

                               -- The title goes in the middle
                               local title = awful.titlebar.widget.titlewidget(c)
                               title:buttons(awful.util.table.join(
                                                awful.button({ }, 1, function()
                                                                client.focus = c
                                                                c:raise()
                                                                awful.mouse.client.move(c)
                                                                     end),
                                                awful.button({ }, 3, function()
                                                                client.focus = c
                                                                c:raise()
                                                                awful.mouse.client.resize(c)
                                                                     end)
                                                                  ))

                               -- Now bring it all together
                               local layout = wibox.layout.align.horizontal()
                               layout:set_left(left_layout)
                               layout:set_right(right_layout)
                               layout:set_middle(title)

                               awful.titlebar(c):set_widget(layout)
                            end
                                   end)

   client.connect_signal("focus", function(c)
                            if not awful.client.ismarked(c) then
                               c.border_color = beautiful.border_focus
                            end
                                  end)
   client.connect_signal("unfocus", function(c)
                            if not awful.client.ismarked(c) then
                               c.border_color = beautiful.border_normal
                            end
                                    end)

   naughty.notify{
      title = "Awesome "..awesome.version.." démarré!",
      text = string.format("Bienvenue %s.\nVous êtes sur la machine \"%s\".\nNous sommes le %s.",
			   os.getenv("USER"), awful.util.pread("hostname"):match("[^\n]*"), os.date("%A %d %B")),
      timeout = 7 }
