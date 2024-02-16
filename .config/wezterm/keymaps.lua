local wezterm = require("wezterm_alt")
local act = wezterm.action
local fmt = wezterm.format
local callback = wezterm.action_callback
local Keys = {}

local function map_leader(key, action) return { key = key, mods = "LEADER", action = action } end

function Keys.apply_to_config(config)
  config.leader = { key = "a", mods = "CTRL", timeout_milliseconds = 1000 }
  config.keys = {
    { key = "a", mods = "LEADER|CTRL", action = act.SendKey({ key = "a", mods = "CTRL" }) },
    map_leader("phys:Space", act.ActivateCommandPalette),
    map_leader("c", act.ActivateCopyMode),
    -- ~ Pane
    map_leader("h", act.ActivatePaneDirection("Left")),
    map_leader("j", act.ActivatePaneDirection("Down")),
    map_leader("k", act.ActivatePaneDirection("Up")),
    map_leader("l", act.ActivatePaneDirection("Right")),
    map_leader("s", act.SplitVertical({ domain = "CurrentPaneDomain" })),
    map_leader("v", act.SplitHorizontal({ domain = "CurrentPaneDomain" })),
    map_leader("q", act.CloseCurrentPane({ confirm = true })),
    map_leader("z", act.TogglePaneZoomState),
    map_leader("o", act.RotatePanes("Clockwise")),
    map_leader("r", act.ActivateKeyTable({ name = "resize_pane", one_shot = false })),
    -- ~ Tab
    map_leader("t", act.SpawnTab("CurrentPaneDomain")),
    map_leader("[", act.ActivateTabRelative(-1)),
    map_leader("]", act.ActivateTabRelative(1)),
    map_leader("n", act.ShowTabNavigator),
    map_leader(
      "e",
      act.PromptInputLine({
        description = fmt({
          { Attribute = { Intensity = "Bold" } },
          { Foreground = { AnsiColor = "Fuchsia" } },
          { Text = "Renaming Tab Title...:" },
        }),
        action = callback(function(window, pane, line)
          if line then window:active_tab():set_title(line) end
        end),
      })
    ),
    -- Move
    map_leader("m", act.ActivateKeyTable({ name = "move_tab", one_shot = false })),
    { key = "{", mods = "LEADER|SHIFT", action = act.MoveTabRelative(-1) },
    { key = "}", mods = "LEADER|SHIFT", action = act.MoveTabRelative(1) },
    map_leader("w", act.ShowLauncherArgs({ flags = "FUZZY|WORKSPACES" })),
  }

  for i = 1, 9 do
    table.insert(config.keys, {
      key = tostring(i),
      mods = "LEADER",
      action = act.ActivateTab(i - 1),
    })
  end

  config.key_tables = {
    resize_pane = {
      { key = "h", action = act.AdjustPaneSize({ "Left", 1 }) },
      { key = "j", action = act.AdjustPaneSize({ "Down", 1 }) },
      { key = "k", action = act.AdjustPaneSize({ "Up", 1 }) },
      { key = "l", action = act.AdjustPaneSize({ "Right", 1 }) },
      { key = "Escape", action = "PopKeyTable" },
      { key = "Enter", action = "PopKeyTable" },
    },
    move_tab = {
      { key = "h", action = act.MoveTabRelative(-1) },
      { key = "j", action = act.MoveTabRelative(-1) },
      { key = "k", action = act.MoveTabRelative(1) },
      { key = "l", action = act.MoveTabRelative(1) },
      { key = "Escape", action = "PopKeyTable" },
      { key = "Enter", action = "PopKeyTable" },
    },
  }
end

return Keys
