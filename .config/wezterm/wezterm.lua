local W = require("wezterm")
local colorscheme = "ayu"
local act = W.action
local fmt = W.format
local nf = W.nerdfonts
local fmttimestr = W.strftime
local kanagawa = require("kanagawa")
local function map(key, action) return { key = key, mods = "LEADER", action = action } end

return {
  -- Window
  window_background_opacity = 1,
  window_decorations = "RESIZE",
  window_padding = { left = "0pt", right = "0pt", top = "0pt", bottom = "0pt" },
  use_resize_increments = true,
  adjust_window_size_when_changing_font_size = false,
  enable_scroll_bar = false,
  inactive_pane_hsb = { saturation = 1, brightness = 0.8 },
  -- Colorscheme
  color_scheme = colorscheme,
  colors = {
    cursor_fg = "#000000",
    tab_bar = {
      background = kanagawa.split,
      active_tab = {
        bg_color = kanagawa.brights[3],
        fg_color = kanagawa.background,
        intensity = "Bold",
        underline = "None",
        italic = false,
        strikethrough = false,
      },
      inactive_tab = { bg_color = kanagawa.background, fg_color = kanagawa.foreground },
    },
  },
  default_cursor_style = "SteadyBlock",
  force_reverse_video_cursor = false,
  -- Font
  allow_square_glyphs_to_overflow_width = "Always",
  freetype_load_flags = "NO_HINTING",
  harfbuzz_features = { "zero", "ss01", "cv05" },
  font_dirs = { "/usr/share/fonts/FiraCodeNF", "/usr/share/fonts/JetBrainsNF", "/usr/share/fonts/TTF" },
  font = W.font_with_fallback({
    { family = "Iosevka Nerd Font Mono Regular" },
    { family = "JetBrainsMonoNL Nerd Font Mono Regular" },
    { family = "FiraCode Nerd Font Mono Regular" },
    { family = "Hasklug Nerd Font Mono Medium" },
    { family = "Symbols Nerd Font" },
    { family = "Symbols Nerd Font Mono" },
  }),
  font_size = 14,
  line_height = 1,
  -- Workspace
  default_workspace = "home",
  -- Performance
  animation_fps = 1,
  max_fps = 60,
  scrollback_lines = 2000,
  audible_bell = "Disabled",
  set_environment_variables = { CURRENT_TERM = "wezterm" },
  -- ~ -------------------------------------------------------------------------------- ~ --
  -- Keys
  leader = { key = "q", mods = "CTRL", timeout_milliseconds = 1000 },
  keys = {
    map("phys:Space", act.ActivateCommandPalette),
    map("c", act.ActivateCopyMode),
    -- ~ Pane
    map("h", act.ActivatePaneDirection("Left")),
    map("j", act.ActivatePaneDirection("Down")),
    map("k", act.ActivatePaneDirection("Up")),
    map("l", act.ActivatePaneDirection("Right")),
    map("s", act.SplitVertical({ domain = "CurrentPaneDomain" })),
    map("v", act.SplitHorizontal({ domain = "CurrentPaneDomain" })),
    map("x", act.CloseCurrentPane({ confirm = true })),
    map("z", act.TogglePaneZoomState),
    map("o", act.RotatePanes("Clockwise")),
    map("r", act.ActivateKeyTable({ name = "resize_pane", one_shot = false })),
    -- ~ Tab
    map("t", act.SpawnTab("CurrentPaneDomain")),
    map("[", act.ActivateTabRelative(-1)),
    map("]", act.ActivateTabRelative(1)),
    map("n", act.ShowTabNavigator),
    -- Move
    map("m", act.ActivateKeyTable({ name = "move_tab", one_shot = false })),
    { key = "{", mods = "LEADER|SHIFT", action = act.MoveTabRelative(-1) },
    { key = "}", mods = "LEADER|SHIFT", action = act.MoveTabRelative(1) },
    map("w", act.ShowLauncherArgs({ flags = "FUZZY|WORKSPACES" })),
  },
  key_tables = {
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
  },
  -- ~ -------------------------------------------------------------------------------- ~ --
  -- Bar
  tab_max_width = 8,
  enable_tab_bar = true,
  use_fancy_tab_bar = false,
  hide_tab_bar_if_only_one_tab = false,
  tab_bar_at_bottom = true,
  show_new_tab_button_in_tab_bar = false,
  status_update_interval = 3000,
  W.on("update-status", function(window, _)
    local stat_color = kanagawa.indexed[16]
    local stat = window:active_workspace()
    if window:active_key_table() then
      stat = window:active_key_table()
      stat_color = kanagawa.brights[7]
    end
    if window:leader_is_active() then
      stat = "LDR "
      stat_color = kanagawa.ansi[5]
    end
    local time = fmttimestr("%H:%M")
    local battery_percentage = ""
    for _, b in ipairs(W.battery_info()) do
      battery_percentage = string.format("%.0f%%", b.state_of_charge * 100)
    end
    window:set_left_status(fmt({
      { Text = "| " },
      { Foreground = { Color = stat_color } },
      { Text = nf.oct_table .. "  " .. stat },
      { Text = " |" },
    }))
    window:set_right_status(fmt({
      { Text = nf.md_clock .. "  " .. time },
      { Text = " | " },
      { Foreground = { Color = kanagawa.brights[4] } },
      { Text = nf.fa_battery_half .. "  " .. battery_percentage },
      "ResetAttributes",
      { Text = " |" },
    }))
  end),
}
