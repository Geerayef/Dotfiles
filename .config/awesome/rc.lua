-- # --------------------------------------------------------------------------- # --

-- ~  Import


-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
local wibox = require("wibox")
local beautiful = require("beautiful")
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")
require("awful.hotkeys_popup.keys")


-- # --------------------------------------------------------------------------- # --

-- ~  Error handling


if awesome.startup_errors then
    naughty.notify({
        preset = naughty.config.presets.critical,
        title = "Oops, there were errors during startup!",
        text = awesome.startup_errors
    })
end

do
    local in_error = false
    awesome.connect_signal("debug::error", function(err)
        if in_error then return end
        in_error = true

        naughty.notify({
            preset = naughty.config.presets.critical,
            title = "Oops, an error happened!",
            text = tostring(err)
        })
        in_error = false
    end)
end


-- # --------------------------------------------------------------------------- # --

-- ~  Variables


-- beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")
beautiful.init("~/.config/awesome/themes/catpuccin.lua")

TERMINAL = "alacritty"
BROWSER = "firefox"
EDITOR = os.getenv("EDITOR") or "nvim"
EDITOR_CMD = TERMINAL .. " -e " .. EDITOR

SUPER = "Mod4"
ALT = "Mod1"


-- # --------------------------------------------------------------------------- # --

-- ~  Layouts


awful.layout.layouts = {
    awful.layout.suit.spiral.dwindle,
    awful.layout.suit.floating,
    awful.layout.suit.max,
    -- awful.layout.suit.tile,
    -- awful.layout.suit.tile.left,
    -- awful.layout.suit.tile.bottom,
    -- awful.layout.suit.tile.top,
    -- awful.layout.suit.fair,
    -- awful.layout.suit.fair.horizontal,
    -- awful.layout.suit.spiral,
    -- awful.layout.suit.max.fullscreen,
    -- awful.layout.suit.magnifier,
    -- awful.layout.suit.corner.nw,
    -- awful.layout.suit.corner.ne,
    -- awful.layout.suit.corner.sw,
    -- awful.layout.suit.corner.se,
}


-- # --------------------------------------------------------------------------- # --

-- ~  Notifications


naughty.config.defaults.ontop = true
naughty.config.defaults.screen = awful.screen.focused()
naughty.config.defaults.timeout = 4
naughty.config.defaults.title = "Notification"
naughty.config.defaults.position = "top_right"
naughty.config.defaults.border_width = 0
beautiful.notification_spacing = 16

-- # --------------------------------------------------------------------------- # --

-- ~  Bar components


-- myawesomemenu = {
--     { "hotkeys",     function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
--     { "manual",      terminal .. " -e man awesome" },
--     { "edit config", editor_cmd .. " " .. awesome.conffile },
--     { "restart",     awesome.restart },
--     { "quit",        function() awesome.quit() end },
-- }
--
-- mymainmenu = awful.menu({
--     items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
--         { "open terminal", terminal }
--     }
-- })
--
-- mylauncher = awful.widget.launcher({
--     image = beautiful.awesome_icon,
--     menu = mymainmenu
-- })

menubar.utils.TERMINAL = TERMINAL

KEYBOARD_LAYOUT = awful.widget.keyboardlayout()

TEXT_CLOCK = wibox.widget.textclock()

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
    awful.button({}, 1, function(t) t:view_only() end),
    awful.button({ SUPER }, 1, function(t)
        if client.focus then
            client.focus:move_to_tag(t)
        end
    end),
    awful.button({}, 3, awful.tag.viewtoggle),
    awful.button({ SUPER }, 3, function(t)
        if client.focus then
            client.focus:toggle_tag(t)
        end
    end),
    awful.button({}, 4, function(t) awful.tag.viewnext(t.screen) end),
    awful.button({}, 5, function(t) awful.tag.viewprev(t.screen) end)
)

-- local tasklist_buttons = gears.table.join(
--     awful.button({}, 1, function(c)
--         if c == client.focus then
--             c.minimized = true
--         else
--             c:emit_signal(
--                 "request::activate",
--                 "tasklist",
--                 { raise = true }
--             )
--         end
--     end),
--     awful.button({}, 3, function()
--         awful.menu.client_list({ theme = { width = 250 } })
--     end),
--     awful.button({}, 4, function()
--         awful.client.focus.byidx(1)
--     end),
--     awful.button({}, 5, function()
--         awful.client.focus.byidx(-1)
--     end))


-- # --------------------------------------------------------------------------- # --

-- ~  Wallpaper & Bar


local function set_wallpaper(s)
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, true)
    end
end

screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
    set_wallpaper(s)

    awful.tag({ "1", "2", "3", "4", "5" }, s, awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(gears.table.join(
        awful.button({}, 1, function() awful.layout.inc(1) end),
        awful.button({}, 3, function() awful.layout.inc(-1) end),
        awful.button({}, 4, function() awful.layout.inc(1) end),
        awful.button({}, 5, function() awful.layout.inc(-1) end)))
    s.mytaglist = awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.all,
        buttons = taglist_buttons
    }

    -- s.mytasklist = awful.widget.tasklist {
    --     screen  = s,
    --     filter  = awful.widget.tasklist.filter.currenttags,
    --     buttons = tasklist_buttons
    -- }

    s.mywibox = awful.wibar({ position = "top", screen = s })
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        -- Left widgets
        {
            layout = wibox.layout.fixed.horizontal,
            -- mylauncher,
            s.mytaglist,
            s.mypromptbox,
        },
        -- Middle widget
        {
            -- s.mytasklist,
            layout = wibox.layout.fixed.horizontal
        },
        -- Right widgets
        {
            layout = wibox.layout.fixed.horizontal,
            KEYBOARD_LAYOUT,
            wibox.widget.systray(),
            TEXT_CLOCK,
            s.mylayoutbox,
        },
    }
end)

-- root.buttons(gears.table.join(
--     awful.button({}, 3, function() mymainmenu:toggle() end),
--     awful.button({}, 4, awful.tag.viewnext),
--     awful.button({}, 5, awful.tag.viewprev)
-- ))


-- # --------------------------------------------------------------------------- # --

-- ~  Key bindings


GLOBAL_KEYS = gears.table.join(
    -- Awesome
    awful.key({ SUPER, "Control" }, "r", awesome.restart,
        { description = "Reload awesome", group = "awesome" }),
    awful.key({ SUPER, "Shift" }, "q", awesome.quit,
        { description = "Quit awesome", group = "awesome" }),
    awful.key({ SUPER, "Control"}, "/", hotkeys_popup.show_help,
        { description = "Show help", group = "awesome" }),
    awful.key({ SUPER }, "r", function() awful.screen.focused().mypromptbox:run() end,
        { description = "Run prompt", group = "launcher" }),
    awful.key({ SUPER }, "x",
        function()
            awful.prompt.run {
                prompt       = "Run Lua code: ",
                textbox      = awful.screen.focused().mypromptbox.widget,
                exe_callback = awful.util.eval,
                history_path = awful.util.get_cache_dir() .. "/history_eval"
            }
        end,
        { description = "Lua: execute prompt", group = "awesome" }),
    awful.key({ SUPER }, "p", function() menubar.show() end,
        { description = "Show the menubar", group = "launcher" }),

    -- Navigation
    awful.key({ ALT }, "h", awful.tag.viewprev,
        { description = "Previous workspace", group = "tag" }),
    awful.key({ ALT }, "l", awful.tag.viewnext,
        { description = "Next workspace", group = "tag" }),
    awful.key({ SUPER, }, "Escape", awful.tag.history.restore,
        { description = "Go back", group = "tag" }),
    awful.key({ SUPER, }, "j", function() awful.client.focus.byidx(1) end,
        { description = "Focus next by index", group = "client" }),
    awful.key({ SUPER, }, "k", function() awful.client.focus.byidx(-1) end,
        { description = "Focus previous by index", group = "client" }),
    awful.key({ SUPER, "Control" }, "j", function() awful.screen.focus_relative(1) end,
        { description = "focus the next screen", group = "screen" }),
    awful.key({ SUPER, "Control" }, "k", function() awful.screen.focus_relative(-1) end,
        { description = "focus the previous screen", group = "screen" }),
    awful.key({ SUPER, }, "u", awful.client.urgent.jumpto,
        { description = "jump to urgent client", group = "client" }),
    awful.key({ SUPER, }, "Tab",
        function()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        { description = "go back", group = "client" }),

    -- Apps
    awful.key({ ALT, "Control" }, "a", function() awful.spawn(TERMINAL) end,
        { description = "Open terminal", group = "launcher" }),
    awful.key({ SUPER, }, "b", function() awful.spawn(BROWSER) end,
        { description = "Open terminal", group = "launcher" }),

    -- Client manipulation
    awful.key({ SUPER, "Shift" }, "j", function() awful.client.swap.byidx(1) end,
        { description = "swap with next client by index", group = "client" }),
    awful.key({ SUPER, "Shift" }, "k", function() awful.client.swap.byidx(-1) end,
        { description = "Swap with previous client by index", group = "client" }),
    awful.key({ SUPER, }, "l", function() awful.tag.incmwfact(0.05) end,
        { description = "Increase master width factor", group = "layout" }),
    awful.key({ SUPER, }, "h", function() awful.tag.incmwfact(-0.05) end,
        { description = "Decrease master width factor", group = "layout" }),
    awful.key({ SUPER, "Shift" }, "h", function() awful.tag.incnmaster(1, nil, true) end,
        { description = "Increase the number of master clients", group = "layout" }),
    awful.key({ SUPER, "Shift" }, "l", function() awful.tag.incnmaster(-1, nil, true) end,
        { description = "Decrease the number of master clients", group = "layout" }),
    awful.key({ SUPER, "Control" }, "h", function() awful.tag.incncol(1, nil, true) end,
        { description = "Increase the number of columns", group = "layout" }),
    awful.key({ SUPER, "Control" }, "l", function() awful.tag.incncol(-1, nil, true) end,
        { description = "Decrease the number of columns", group = "layout" }),
    awful.key({ SUPER, }, "space", function() awful.layout.inc(1) end,
        { description = "Select next", group = "layout" }),
    awful.key({ SUPER, "Shift" }, "space", function() awful.layout.inc(-1) end,
        { description = "Select previous", group = "layout" }),
    awful.key({ SUPER, "Control" }, "n",
        function()
            local c = awful.client.restore()
            if c then
                c:emit_signal(
                    "request::activate", "key.unminimize", { raise = true }
                )
            end
        end,
        { description = "Restore minimized", group = "client" })
)

CLIENT_KEYS = gears.table.join(
    awful.key({ SUPER, "Control" }, "f",
        function(c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        { description = "Toggle fullscreen", group = "client" }),
    awful.key({ SUPER, }, "q", function(c) c:kill() end,
        { description = "Quit", group = "client" }),
    awful.key({ SUPER, "Control" }, "space", awful.client.floating.toggle,
        { description = "Toggle floating", group = "client" }),
    awful.key({ SUPER, "Control" }, "Return", function(c) c:swap(awful.client.getmaster()) end,
        { description = "Move to master", group = "client" }),
    awful.key({ SUPER, "Control" }, "o", function(c) c:move_to_screen() end,
        { description = "Move to screen", group = "client" }),
    awful.key({ SUPER, "Control" }, "t", function(c) c.ontop = not c.ontop end,
        { description = "Toggle keep on top", group = "client" }),
    awful.key({ SUPER, }, "n", function(c) c.minimized = true end,
        { description = "minimize", group = "client" }),
    awful.key({ SUPER, }, "m",
        function(c)
            c.maximized = not c.maximized
            c:raise()
        end,
        { description = "(un)maximize", group = "client" }),
    awful.key({ SUPER, "Control" }, "m",
        function(c)
            c.maximized_vertical = not c.maximized_vertical
            c:raise()
        end,
        { description = "(un)maximize vertically", group = "client" }),
    awful.key({ SUPER, "Shift" }, "m",
        function(c)
            c.maximized_horizontal = not c.maximized_horizontal
            c:raise()
        end,
        { description = "(un)maximize horizontally", group = "client" })
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    GLOBAL_KEYS = gears.table.join(GLOBAL_KEYS,
        -- View tag only.
        awful.key({ SUPER }, "#" .. i + 9,
            function()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                    tag:view_only()
                end
            end,
            { description = "view tag #" .. i, group = "tag" }),
        -- Toggle tag display.
        awful.key({ SUPER, "Control" }, "#" .. i + 9,
            function()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                    awful.tag.viewtoggle(tag)
                end
            end,
            { description = "toggle tag #" .. i, group = "tag" }),
        -- Move client to tag.
        awful.key({ SUPER, "Shift" }, "#" .. i + 9,
            function()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:move_to_tag(tag)
                    end
                end
            end,
            { description = "move focused client to tag #" .. i, group = "tag" }),
        -- Toggle tag on focused client.
        awful.key({ SUPER, "Control", "Shift" }, "#" .. i + 9,
            function()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:toggle_tag(tag)
                    end
                end
            end,
            { description = "toggle focused client on tag #" .. i, group = "tag" })
    )
end

CLIENT_BUTTONS = gears.table.join(
    awful.button({}, 1, function(c)
        c:emit_signal("request::activate", "mouse_click", { raise = true })
    end),
    awful.button({ SUPER }, 1, function(c)
        c:emit_signal("request::activate", "mouse_click", { raise = true })
        awful.mouse.client.move(c)
    end),
    awful.button({ SUPER }, 3, function(c)
        c:emit_signal("request::activate", "mouse_click", { raise = true })
        awful.mouse.client.resize(c)
    end)
)

-- Set keys
root.keys(GLOBAL_KEYS)


-- # --------------------------------------------------------------------------- # --

-- ~  Window rules:
-- Rules to apply to new clients (through the "manage" signal).


awful.rules.rules = {
    -- All clients will match this rule.
    {
        rule = {},
        properties = {
            border_width = beautiful.border_width,
            border_color = beautiful.border_normal,
            focus = awful.client.focus.filter,
            raise = true,
            keys = CLIENT_KEYS,
            -- buttons = CLIENT_BUTTONS,
            screen = awful.screen.preferred,
            placement = awful.placement.no_overlap + awful.placement.no_offscreen
        }
    },

    -- Floating clients.
    {
        rule_any = {
            instance = {
                "DTA", -- Firefox addon DownThemAll.
                "copyq", -- Includes session name in class.
                "pinentry",
            },
            class = {
                "Arandr",
                "Blueman-manager",
                "Gpick",
                "Kruler",
                "MessageWin", -- kalarm.
                "Sxiv",
                "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
                "Wpa_gui",
                "veromix",
                "xtightvncviewer" },

            -- Note that the name property shown in xprop might be set slightly after creation of the client
            -- and the name shown there might not match defined rules here.
            name = {
                "Event Tester", -- xev.
            },
            role = {
                "AlarmWindow", -- Thunderbird's calendar.
                "ConfigManager", -- Thunderbird's about:config.
                "pop-up",  -- e.g. Google Chrome's (detached) Developer Tools.
            }
        },
        properties = { floating = true }
    },

    -- Add titlebars to normal clients and dialogs
    {
        rule_any = { type = { "normal", "dialog" }
        },
        properties = { titlebars_enabled = false }
    },

    -- Set Firefox to always map on the tag named "2" on screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { screen = 1, tag = "2" } },
}


-- # --------------------------------------------------------------------------- # --

-- ~  Signals
-- Signal function to execute when a new client appears.


client.connect_signal("manage", function(c)
    if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup
        and not c.size_hints.user_position
        and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Titlebar ( titlebars_enabled? )
client.connect_signal("request::titlebars", function(c)
    local buttons = gears.table.join(
        awful.button({}, 1, function()
            c:emit_signal("request::activate", "titlebar", { raise = true })
            awful.mouse.client.move(c)
        end),
        awful.button({}, 3, function()
            c:emit_signal("request::activate", "titlebar", { raise = true })
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c):setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        {     -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton(c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton(c),
            awful.titlebar.widget.ontopbutton(c),
            awful.titlebar.widget.closebutton(c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    c:emit_signal("request::activate", "mouse_enter", { raise = false })
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

awful.spawn.with_shell("picom")
awful.spawn.with_shell("nitrogen --restore &")
