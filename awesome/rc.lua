-- File: rc.lua
-- Author: Nicolas G. Querol

require("awful")
require("awful.autofocus")
require("awful.rules")
require("vicious")
require("beautiful")

beautiful.init("/home/nico/.config/awesome/theme.lua")

os.setlocale("fr_FR.UTF-8")

local home = os.getenv("HOME")
local terminal = "urxvtc"
local modkey = "Mod4"

layouts =
   {
   awful.layout.suit.floating,
   awful.layout.suit.tile,
   awful.layout.suit.tile.left,
   awful.layout.suit.tile.bottom,
   awful.layout.suit.tile.top,
   awful.layout.suit.fair,
   awful.layout.suit.fair.horizontal,
   }

   tags = {}
   for s = 1, screen.count() do
      tags[s] = awful.tag({ "main", "web", "dev" }, s, layouts[1])
   end



   mymainmenu = awful.menu({ items = {
				{ "Files", "thunar" },
				{ "", },
				{"Terminal", "urxvtc"},
				{ "", },
				{ "Eclipse", "eclipse" },
				{ "Emacs", "emacsclient -c" },
				{ "Evince", "evince" },
				{ "Firefox", "firefox"},
				{ "GIMP", "gimp" },
				{ "Transmission", "transmission-gtk"},
				{ "", },
				{ "Restart",  "sudo reboot" },
				{ "Shutdown", "sudo shutdown -h now" },
				{ "Sleep", "sudo pm-suspend-hybrid" }
				     }})

   mylauncher = awful.widget.launcher({ image = image(beautiful.awesome_icon),
					menu = mymainmenu })

   -- Date and time
   dateicon = widget({ type = "imagebox" })
   dateicon.image = image(beautiful.widget_date)
   datewidget = widget({ type = "textbox" })
   vicious.register(datewidget, vicious.widgets.date, "%d/%m/%y,<span weight='bold'> %R</span>", 60)

   -- Spacers
   spacer1 = widget({ type = "textbox" })
   spacer1.text = " | "
   spacer2 = widget({ type = "textbox" })
   spacer2.text = " "

   -- Left side
   lside = widget({ type = "imagebox" })
   lside.image = image(beautiful.widget_left)

   -- Right side
   rside = widget({ type = "imagebox" })
   rside.image = image(beautiful.widget_right)

   -- Gmail widget & tooltip
   gmailicon = widget({ type = "imagebox" })
   gmailicon.image = image(beautiful.widget_mail)
   gmail_t = awful.tooltip({ objects = { gmailwidget },})
   gmailwidget = widget({ type = "textbox" })
   vicious.register(gmailwidget, vicious.widgets.gmail,
		    function (widget, args)
		       gmail_t:set_text(" <span weight='bold' color='#66AABB'>"..args["{sender}"].."</span>: "..args["{subject}"].." ")
		       gmail_t:add_to_object(gmailicon)
		       if args['{count}'] > 0 then
			  return "<span color ='#66AABB' weight ='bold'>"..args["{count}"].."</span>"
		       else
			  return args['{count}']
		       end
		    end, 69)

   gmailicon:buttons(awful.util.table.join(
			awful.button({ }, 1, function () awful.util.spawn_with_shell("urxvtc -geometry 100x25 -e mutt") end)))

   -- Battery percentage & state
   baticon = widget({ type = "imagebox" })
   baticon.image = image(beautiful.widget_bat)
   batwidget = widget({ type = "textbox", align = "right" })
   vicious.register(batwidget, vicious.widgets.bat,
		    function (widget, args)
		       -- full/charged
		       if args[1] == "↯" then
			  return "<span color='#9EDB58'><span weight='bold'>"..args[2].."</span>%</span>"
		       else
			  -- charging
			  if args[1] == "+" then
			     return "<span weight='bold'>"..args[1]..args[2].."</span>% ("..args[3]..")"
			  else
			     -- low battery
			     if args[2] <= 10 then
				return "<span color='#E84F4F'><span weight='bold'>"..args[2].."</span>%</span>"
			     else
				-- discharging
				return "<span weight='bold'>"..args[2].."</span>%"
			     end
			  end
		       end
		    end, 61, "BAT0")

   -- CPU usage
   cpuicon = widget({ type = "imagebox" })
   cpuicon.image = image(beautiful.widget_cpu)
   cpuwidget = widget({ type = "textbox", align = "right" })
   vicious.register(cpuwidget, vicious.widgets.cpu, "<span weight='bold'>$1</span>%", 2)

   -- CPU temperature
   thermalwidget = widget({ type = "textbox" })
   vicious.register(thermalwidget, vicious.widgets.thermal,
		    function (widget, args)
		       if args[1] >= 70 then
			  return "<span weight='bold'>, <span color='#E84F4F'>"..args[1].."°C</span></span>"
		       else
			  return " @ <span weight='bold'>"..args[1].."</span>°C"
		       end
		    end, 19, {"coretemp.0", "core"})

   mysystray = widget({ type = "systray" })

   mywibox = {}
   mypromptbox = {}
   mylayoutbox = {}
   mytaglist = {}
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
			   end))

   for s = 1, screen.count() do
      mypromptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })


      mylayoutbox[s] = awful.widget.layoutbox(s)
      mylayoutbox[s]:buttons(awful.util.table.join(
				awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
				awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
				awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
				awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))

      mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all, mytaglist.buttons)

      mytasklist[s] = awful.widget.tasklist(function(c)
					       return awful.widget.tasklist.label.currenttags(c, s)
					    end, mytasklist.buttons)

      mywibox[s] = awful.wibox({ position = "bottom", height = "18", screen = s })
      mywibox[s].widgets = {
	 {
	    mylauncher,
	    spacer2,
	    mytaglist[s],
	    spacer2,
	    mylayoutbox[s],
	    spacer2,
	    mypromptbox[s],
	    spacer2,
	    layout = awful.widget.layout.horizontal.leftright
	 },
	 rside,
	 spacer2,
	 s == 1 and mysystray or nil,
	 spacer1,
	 datewidget,
	 dateicon,
	 spacer1,
	 gmailwidget,
	 gmailicon,
	 spacer1,
	 batwidget,
	 baticon,
	 spacer1,
	 thermalwidget,
	 cpuwidget,
	 cpuicon,
	 spacer2,
	 lside,
	 spacer2,
	 --	 mytasklist[s],
	 layout = awful.widget.layout.horizontal.rightleft
      }
   end

   root.buttons(
      awful.button({ }, 3, function () mymainmenu:toggle() end)
	       )

   globalkeys = awful.util.table.join(
      awful.key({ modkey,           }, "e",  function () awful.util.spawn("emacsclient -c") end),
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
		   if client.focus then
		      client.focus:raise()
		   end
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
		end))

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
						   end))
   end

   clientbuttons = awful.util.table.join(
      awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
      awful.button({ modkey }, 1, awful.mouse.client.move),
      awful.button({ modkey }, 3, awful.mouse.client.resize))

   root.keys(globalkeys)

   awful.rules.rules = {
      { rule = { },
	properties = { border_width = beautiful.border_width,
		       border_color = beautiful.border_normal,
		       focus = true,
		       keys = clientkeys,
		       size_hints_honor = false,
		       buttons = clientbuttons } },

      { rule = { class = "Firefox" },
	properties = { tag = tags[1][2] }, { floating = "false" } },
      { rule = { name = "Téléchargements" },
	properties = { floating = "true" } },
      { rule = { class = "Eclipse" },
	properties = { tag = tags[1][3] } },
   }

   client.add_signal("manage", function (c, startup)

			c:add_signal("mouse::enter", function(c)
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
