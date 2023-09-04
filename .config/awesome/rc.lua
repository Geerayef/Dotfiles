-- # --------------------------------------------------------------------------- # --

-- ~  Imports


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
    awesome.connect_signal(
        "debug::error",
        function(err)
            if in_error then return end
            in_error = true

            naughty.notify({
                preset = naughty.config.presets.critical,
                title = "Oops, an error happened!",
                text = tostring(err)
            })
            in_error = false
        end
    )
end


-- # --------------------------------------------------------------------------- # --

-- ~  Variables


TERMINAL = "alacritty"
BROWSER = "firefox"
EDITOR = os.getenv("EDITOR") or "nvim"
EDITOR_CMD = TERMINAL .. " -e " .. EDITOR

SUPER = "Mod4"
ALT = "Mod1"

menubar.utils.TERMINAL = TERMINAL

KEYBOARD_LAYOUT = awful.widget.keyboardlayout()
TEXT_CLOCK = wibox.widget.textclock()

local workspace_buttons = gears.table.join(
    awful.button({}, 1, function(w) w:view_only() end),
    awful.button(
        { SUPER }, 1,
        function(w)
            if client.focus then
                client.focus:move_to_tag(w)
            end
        end
    ),
    awful.button({}, 3, awful.tag.viewtoggle),
    awful.button(
        { SUPER }, 3,
        function(w)
            if client.focus then
                client.focus:toggle_tag(w)
            end
        end
    ),
    awful.button({}, 4, function(w) awful.tag.viewnext(w.screen) end),
    awful.button({}, 5, function(w) awful.tag.viewprev(w.screen) end)
)


-- # --------------------------------------------------------------------------- # --

-- ~  Layouts


awful.layout.layouts = {
    awful.layout.suit.spiral.dwindle,
    awful.layout.suit.tile,
    awful.layout.suit.floating,
    -- awful.layout.suit.max,
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

-- ~  Screen, Wallpaper & Bar


-- beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")
beautiful.init("~/.config/awesome/themes/catpuccin.lua")

-- Wallpaper
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
        buttons = workspace_buttons
    }

    -- # -------------------- Bar -------------------- # --

    s.mybar = awful.wibar({ position = "top", screen = s })
    s.mybar:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left
            layout = wibox.layout.fixed.horizontal,
            s.mytaglist,
            s.mypromptbox,
        },
        { -- Middle
            layout = wibox.layout.fixed.horizontal
        },
        { -- Right
            layout = wibox.layout.fixed.horizontal,
            KEYBOARD_LAYOUT,
            wibox.widget.systray(),
            TEXT_CLOCK,
            s.mylayoutbox,
        },
    }
end)


-- # --------------------------------------------------------------------------- # --

-- ~  Key bindings


-- Bindings are groupped by:
-- - mod keys [a-z][# of mods: ascending]
-- - the domain of effect

GLOBAL_KEYS = gears.table.join(

    -- # -------------------- Awesome WM -------------------- # --

    awful.key(
        { SUPER }, "r",
        function() awful.screen.focused().mypromptbox:run() end,
        { description = "[R]un prompt", group = "launcher" }
    ),

    awful.key(
        { SUPER }, "x",
        function()
            awful.prompt.run {
                prompt       = "Evaluate Lua code: ",
                textbox      = awful.screen.focused().mypromptbox.widget,
                exe_callback = awful.util.eval,
                history_path = awful.util.get_cache_dir() .. "/history_eval"
            }
        end,
        { description = "Lua: e[x]ecute prompt", group = "awesome" }
    ),

    awful.key(
        { SUPER }, "p",
        function() menubar.show() end,
        { description = "Show [p]rogram launcher", group = "launcher" }
    ),

    awful.key(
        { SUPER, ALT }, "r",
        awesome.restart,
        { description = "[R]eload awesome", group = "awesome" }
    ),

    awful.key(
        { SUPER, ALT }, "/",
        hotkeys_popup.show_help,
        { description = "Show help", group = "awesome" }
    ),

    awful.key(
        { SUPER, ALT }, "q",
        awesome.quit,
        { description = "[Q]uit awesome", group = "awesome" }
    ),

    -- # -------------------- Navigation -------------------- # --

    awful.key(
        { ALT }, "h",
        awful.tag.viewprev,
        { description = "Previous workspace", group = "tag" }
    ),

    awful.key(
        { ALT }, "l",
        awful.tag.viewnext,
        { description = "Next workspace", group = "tag" }
    ),

    awful.key(
        { ALT }, "Tab",
        function()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        { description = "Focus previous client", group = "client" }
    ),

    awful.key(
        { SUPER }, "Tab",
        awful.tag.history.restore,
        { description = "Back to previous workspace", group = "tag" }
    ),

    awful.key(
        { SUPER }, "u",
        awful.client.urgent.jumpto,
        { description = "Jump to [u]rgent client", group = "client" }
    ),

    awful.key(
        { SUPER }, "h",
        function(c)
            awful.client.focus.global_bydirection("left")
            c:lower()
        end,
        {description = "Focus client left", group = "client"}
    ),

    awful.key(
        { SUPER }, "j",
        function(c)
            awful.client.focus.global_bydirection("down")
            c:lower()
        end,
        {description = "Focus client down", group = "client"}
    ),

    awful.key(
        { SUPER }, "k",
        function(c)
            awful.client.focus.global_bydirection("up")
            c:lower()
        end,
        {description = "Focus client up", group = "client"}
    ),

    awful.key(
        { SUPER }, "l",
        function(c)
            awful.client.focus.global_bydirection("right")
            c:lower()
        end,
        {description = "Focus client right", group = "client"}
    ),

    awful.key(
        { SUPER, "Shift" }, "h",
        function (c)
            awful.client.swap.global_bydirection("left")
            c:raise()
        end,
        {description = "Move client left", group = "client"}
    ),

    awful.key(
        { SUPER, "Shift" }, "j",
        function (c)
            awful.client.swap.global_bydirection("down")
            c:raise()
        end,
        {description = "Move client down", group = "client"}
    ),

    awful.key(
        { SUPER, "Shift" }, "k",
        function (c)
            awful.client.swap.global_bydirection("up")
            c:raise()
        end,
        {description = "Move client up", group = "client"}
    ),

    awful.key(
        { SUPER, "Shift" }, "l",
        function (c)
            awful.client.swap.global_bydirection("right")
            c:raise()
        end,
        {description = "Move client right", group = "client"}
    ),

    -- # -------------------- Apps -------------------- # --

    awful.key(
        { ALT, "Control" }, "a",
        function() awful.spawn(TERMINAL) end,
        { description = "Open terminal", group = "launcher" }
    ),

    awful.key(
        { SUPER, }, "b",
        function() awful.spawn(BROWSER) end,
        { description = "Open browser", group = "launcher" }
    ),

    -- # -------------------- Layout -------------------- # --

    awful.key(
        { SUPER }, "space",
        function() awful.layout.inc(1) end,
        { description = "Select next layout", group = "layout" }
    ),

    -- # -------------------- Client -------------------- # --

    awful.key(
        { SUPER, "Shift" }, "n",
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

    -- # -------------------- Sizing -------------------- # --

    awful.key(
        { ALT, "Control" }, "m",
        function(c) c:swap(awful.client.getmaster()) end,
        { description = "Move to [m]aster", group = "client" }
    ),

    awful.key(
        { SUPER }, "n",
        function(c) c.minimized = true end,
        { description = "Mi[n]imize", group = "client" }
    ),

    awful.key(
        { SUPER }, "f",
        function(c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        { description = "Toggle [f]ullscreen", group = "client" }
    ),

    awful.key(
        { SUPER }, "m",
        function(c)
            c.maximized = not c.maximized
            c:raise()
        end,
        { description = "Toggle [m]aximize", group = "client" }
    ),

    awful.key(
        { SUPER, "Control" }, "m",
        function(c)
            c.maximized_vertical = not c.maximized_vertical
            c:raise()
        end,
        { description = "Toggle maximize vertically", group = "client" }
    ),

    awful.key(
        { SUPER, "Shift" }, "m",
        function(c)
            c.maximized_horizontal = not c.maximized_horizontal
            c:raise()
        end,
        { description = "Toggle maximize horizontally", group = "client" }
    ),

    -- # -------------------- Control -------------------- # --

    awful.key(
        { SUPER }, "q",
        function(c) c:kill() end,
        { description = "Quit", group = "client" }
    ),

    awful.key(
        { SUPER, "Control" }, "f",
        awful.client.floating.toggle,
        { description = "Toggle [f]loating", group = "client" }
    ),

    awful.key(
        { SUPER, "Control" }, "o",
        function(c) c:move_to_screen() end,
        { description = "Move to screen", group = "client" }
    ),

    awful.key(
        { SUPER, "Control" }, "p",
        function(c) c.ontop = not c.ontop end,
        { description = "Toggle [p]in on top", group = "client" }
    )
)

-- # -------------------- Bind numkeys to workspaces -------------------- # --
-- !!! We use keycodes to make it work on any keyboard layout. !!!

for i = 1, 5 do
    GLOBAL_KEYS = gears.table.join(GLOBAL_KEYS,

        awful.key(
            { SUPER }, "#" .. i + 9,
            function()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                    tag:view_only()
                end
            end,
            { description = "Go to workspace #" .. i, group = "tag" }
        ),

        awful.key(
            { SUPER, "Control" }, "#" .. i + 9,
            function()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                    awful.tag.viewtoggle(tag)
                end
            end,
            { description = "Mark workspace #" .. i, group = "tag" }
        ),

        awful.key(
            { SUPER, "Shift" }, "#" .. i + 9,
            function()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:move_to_tag(tag)
                    end
                end
            end,
            { description = "Move focused client to workspace #" .. i, group = "tag" }
        ),

        awful.key({ SUPER, "Control", "Shift" }, "#" .. i + 9,
            function()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:toggle_tag(tag)
                    end
                end
            end,
            { description = "Toggle focused client in workspace #" .. i, group = "tag" }
        )
    )
end

root.keys(GLOBAL_KEYS)


-- # --------------------------------------------------------------------------- # --

-- ~  Mouse buttons


CLIENT_BUTTONS = gears.table.join(
    awful.button(
        {}, 1,
        function(c)
            c:emit_signal("request::activate", "mouse_click", { raise = true })
        end
    ),

    awful.button(
        { SUPER }, 1,
        function(c)
            c:emit_signal("request::activate", "mouse_click", { raise = true })
            awful.mouse.client.move(c)
        end
    ),

    awful.button(
        { SUPER }, 3,
        function(c)
            c:emit_signal("request::activate", "mouse_click", { raise = true })
            awful.mouse.client.resize(c)
        end
    )
)


-- # --------------------------------------------------------------------------- # --

-- ~  Window rules:
-- !!! Applies to new clients - through the signal: "manage" !!!


awful.rules.rules = {
    -- All
    {
        rule = {},
        properties = {
            border_width = beautiful.border_width,
            border_color = beautiful.border_normal,
            focus = awful.client.focus.filter,
            raise = true,
            keys = CLIENT_KEYS,
            screen = awful.screen.preferred,
            placement = awful.placement.no_overlap + awful.placement.no_offscreen
        }
    },

    -- Floating
    {
        rule_any = {
            instance = {},
            class = {
                "Arandr",
                "Blueman-manager",
                "Gpick",
                "Wpa_gui",
            },

            -- Note that the name property shown in xprop might be set slightly after creation of the client
            -- and the name shown there might not match defined rules here.
            name = {
                -- "Event Tester", -- xev.
            },
            role = {
                -- "pop-up",  -- e.g. Google Chrome's (detached) Developer Tools.
            }
        },
        properties = { floating = true }
    },

    -- Titlebar rule
    {
        rule_any = { type = { "normal", "dialog" } },
        properties = { titlebars_enabled = false }
    },

    -- Map apps to workspaces
    -- {
    --     rule = { class = "Firefox" },
    --     properties = { screen = 1, tag = "2" },
    -- },
}


-- # --------------------------------------------------------------------------- # --

-- ~  Signals
-- On create-client functions


client.connect_signal("manage", function(c)
    if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup
        and not c.size_hints.user_position
        and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Titlebar signal
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

-- Sloppy focus: mouse focus
client.connect_signal(
    "mouse::enter",
    function(c)
        c:emit_signal("request::activate", "mouse_enter", { raise = false })
    end
)
client.connect_signal(
    "focus",
    function(c) c.border_color = beautiful.border_focus end
)
client.connect_signal(
    "unfocus",
    function(c) c.border_color = beautiful.border_normal end
)


-- # --------------------------------------------------------------------------- # --

-- ~  Signals


awful.spawn.with_shell("picom")
awful.spawn.with_shell("nitrogen --restore &")
