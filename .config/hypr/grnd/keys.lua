local road = __require("grnd.road").base
local map = hl.bind
local sub = hl.dsp.submap
local cmd = hl.dsp.exec_cmd
local focus = hl.dsp.focus
local window = hl.dsp.window
local workspace = hl.dsp.workspace
local group = hl.dsp.group
local flag = {
  lock = { locked = true },
  lock_rep = { locked = true, repeating = true },
  rep = { repeating = true },
  mouse = { mouse = true },
}
local app = {
  browser = { main = "brave", alt = "firefox" },
  terminal = { main = "wezterm", alt = "foot" },
  launcher = "tofi-run | xargs hyprctl dispatch exec --",
  filexplorer = "wezterm start --always-new-process fish --init-command=lf",
  clipboard = (
    "cliphist list | tofi --config "
    .. os.getenv("XDG_CONFIG_HOME")
    .. "/tofi/clipboard | cliphist decode | wl-copy"
  ),
}
local path = {
  scripts = os.getenv("HOME") .. "/.local/bin/scripts/",
}

-- Hyprland
map("SUPER + Escape", cmd("hyprlock"))
map("ALT + CONTROL + SUPER + X", cmd("hyprshutdown -t 'Log out'"))
map("ALT + CONTROL + SUPER + S", cmd("hyprshutdown -t 'Shutting down' -p 'shutdown -P 0'"))
map("ALT + CONTROL + SUPER + R", cmd("hyprshutdown -t 'Restarting' -p 'reboot'"))
do
  local kblayout = { { "GB", "QWERTY" }, { "US", "Dvorak" }, { "SRB", "QWERTY (latin)" } }
  local i = 1
  map("SUPER + Space", function()
    i = i + 1
    if i > 3 then i = 1 end
    cmd("hyprctl switchxkblayout current next")
    hl.notification.create({
      text = table.concat(kblayout[i], " "),
      timeout = 3000,
      icon = 1,
      color = road.dragonInk,
      font_size = 24,
    })
  end, flag.lock)
end

-- Sound
map("XF86AudioMute", cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"), flag.lock_rep)
map("XF86AudioRaiseVolume", cmd("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"), flag.lock_rep)
map("XF86AudioLowerVolume", cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"), flag.lock_rep)

-- Brightness
map("XF86MonBrightnessUp", cmd("brightnessctl -- s +10%"), flag.lock_rep)
map("XF86MonBrightnessDown", cmd("brightnessctl -- s -10%"), flag.lock_rep)

-- Media
map("XF86AudioPlay", cmd("playerctl play-pause"), flag.lock)
map("XF86AudioNext", cmd("playerctl next"), flag.lock)
map("XF86AudioPrev", cmd("playerctl previous"), flag.lock)
map("XF86AudioStop", cmd("playerctl stop"), flag.lock)

-- Window
map("SUPER + Q", window.close())
map("CONTROL + SUPER + F", window.fullscreen())
map("ALT + SUPER + F", window.float({ action = "toggle" }))

-- Submap: resize
map("ALT + SUPER + R", sub("resize"))
hl.define_submap("resize", function()
  map("l", window.resize({ x = 20, y = 0, relative = true }), flag.rep)
  map("h", window.resize({ x = -20, y = 0, relative = true }), flag.rep)
  map("k", window.resize({ x = 0, y = -20, relative = true }), flag.rep)
  map("j", window.resize({ x = 0, y = 20, relative = true }), flag.rep)
  map("catchall", sub("reset"))
end)

-- Group
map("SUPER + G", group.toggle())
map("ALT + CONTROL + H", group.prev())
map("ALT + CONTROL + L", group.next())
map("ALT + CONTROL + O", window.move({ out_of_group = true }))
map("ALT + SHIFT + L", window.move({ into_group = "r" }))
map("ALT + SHIFT + H", window.move({ into_group = "l" }))
map("ALT + SHIFT + J", window.move({ into_group = "d" }))
map("ALT + SHIFT + K", window.move({ into_group = "u" }))

-- Apps
map("SUPER + W", cmd(app.terminal.main))
map("SUPER + T", cmd(app.terminal.alt))
map("SUPER + B", cmd(app.browser.main))
map("ALT + SUPER + B", cmd(app.browser.alt))
map("SUPER + F", cmd(app.filexplorer))
map("SUPER + R", cmd("fuzzel"))
map("CONTROL + SUPER + C", cmd(app.clipboard))
map("CONTROL + SUPER + B", cmd(path.scripts .. "bluetooth.bash"))
map("Print", cmd(path.scripts .. "ss.bash"))
map("ALT + Print", cmd(path.scripts .. "ss.bash --selection"))
map("ALT + SUPER + D", cmd("makoctl dismiss"))

-- Focus -------------------------------------------------------------------------------------------
-- window
map("SUPER + H", focus({ direction = "l" }))
map("SUPER + J", focus({ direction = "d" }))
map("SUPER + K", focus({ direction = "u" }))
map("SUPER + L", focus({ direction = "r" }))
-- workspace
map("SUPER + 0", workspace.toggle_special("special:0"))
for i = 1, 5 do
  map("SUPER + " .. i, focus({ workspace = tostring(i) }))
end
map("CONTROL + SUPER + H", focus({ workspace = "e-1" }))
map("CONTROL + SUPER + L", focus({ workspace = "e+1" }))
map("ALT + SUPER + H", focus({ workspace = "-1" }))
map("ALT + SUPER + L", focus({ workspace = "+1" }))

-- Move --------------------------------------------------------------------------------------------
-- to workspace
map("SHIFT + SUPER + 0", window.move({ workspace = "special:0" }))
for i = 1, 5 do
  map("SHIFT + SUPER + " .. i, window.move({ workspace = tostring(i) }))
end
map("SHIFT + SUPER + H", window.move({ workspace = "-1" }))
map("SHIFT + SUPER + L", window.move({ workspace = "+1" }))
-- to monitor
map("ALT + SHIFT + SUPER + H", window.move({ monitor = "l" }))
map("ALT + SHIFT + SUPER + L", window.move({ monitor = "r" }))
-- swap
map("SHIFT + SUPER + R", window.swap({ direction = "r" }))
map("SHIFT + SUPER + E", window.swap({ direction = "l" }))
map("SHIFT + SUPER + S", window.swap({ next = true }))
-- mouse
map("SUPER + mouse:272", window.drag(), flag.mouse)
map("SUPER + mouse:273", window.resize(), flag.mouse)
