-- ~ Global ---------------------------------------------------------------- ~ --

local W = require("wezterm")
local act = require("wezterm").action
local nf = require("wezterm").nerdfonts
local ayu = require("ayu")
local C = {}
local G = W.GLOBAL
if W.config_builder then C = W.config_builder() end

G.icon_proc = {
  ["presenterm"] = nf.fa_hashtag,
  ["opam"] = nf.seti_ocaml,
  ["dune"] = nf.seti_ocaml,
  ["ocamlc"] = nf.seti_ocaml,
  ["cargo"] = nf.dev_rust,
  ["lua"] = nf.seti_lua,
  ["fish"] = nf.fa_terminal,
  ["zsh"] = nf.dev_terminal,
  ["bash"] = nf.cod_terminal_bash,
  ["vim"] = nf.dev_vim,
  ["nvim"] = nf.linux_neovim,
  ["btop"] = nf.md_chart_donut_variant,
  ["git"] = nf.dev_git,
  ["gh"] = nf.dev_github_badge,
  ["sudo"] = nf.fa_hashtag,
}

local font_family = "ZedMono Nerd Font Mono"
local font_features = { "calt=1", "clig=1", "liga=1", "dlig=1" }
if
  string.match(font_family, "Zed") ~= nil
  or string.match(font_family, "Iosevka") ~= nil
then
  font_features =
    { "calt=1", "clig=1", "liga=1", "dlig=1", "cv26=12", "cv85=6", "ss10" }
elseif string.match(font_family, "Fira") ~= nil then
  font_features = {
    "zero",
    "calt=1",
    "clig=1",
    "liga=1",
    "dlig=1",
    "cv01",
    "cv02",
    "cv04",
    "cv08",
    "cv29",
    "cv30",
    "cv31",
    "ss01",
    "ss02",
    "ss05",
    "ss09",
  }
elseif string.match(font_family, "Jet") ~= nil then
  font_features =
    { "calt=1", "clig=1", "liga=1", "dlig=1", "cv04", "cv07", "cv08", "cv17" }
end

-- ~ Function ------------------------------------------------------------- ~ --

local function map(key, action)
  return { key = key, mods = "LEADER", action = action }
end

local function proc_name(tab)
  local proc = tab.active_pane.foreground_process_name:match("([^/\\]+)$")
  return G.icon_proc[proc] or "○ "
end

-- ~ Statusbar ------------------------------------------------------------- ~ --

W.on("format-tab-title", function(tab) return " " .. proc_name(tab) .. " " end)

W.on("update-status", function(window, _)
  local stat_color = ayu.indexed[16]
  local stat = window:active_workspace()
  if window:active_key_table() then
    stat = window:active_key_table()
    if stat == "move_tab" then
      stat = "move"
    elseif stat == "resize_pane" then
      stat = "size"
    elseif stat == "copy_mode" then
      stat = "copy"
    elseif stat == "search_mode" then
      stat = "find"
    end
    stat_color = ayu.brights[5]
  end
  if window:leader_is_active() then
    stat = "LDR "
    stat_color = ayu.ansi[5]
  end
  local time = W.strftime("%H:%M")
  window:set_left_status(W.format({
    { Foreground = { Color = stat_color } },
    { Text = "    " },
    { Text = nf.oct_table .. " " .. stat },
    { Text = " │ " },
    "ResetAttributes",
  }))
  window:set_right_status(W.format({
    { Foreground = { Color = ayu.indexed[16] } },
    { Text = nf.md_clock .. " " .. time },
    "ResetAttributes",
    { Text = "    " },
  }))
end)

-- ~ Option ------------------------------------------------------------- ~ --

-- Bar
C.tab_bar_at_bottom = true
C.use_fancy_tab_bar = false
C.show_new_tab_button_in_tab_bar = false
C.status_update_interval = 1000

-- Window
C.window_background_opacity = 1
C.window_decorations = "NONE"
C.window_padding = { left = "0pt", right = "0pt", top = "0pt", bottom = "0pt" }
C.use_resize_increments = true
C.adjust_window_size_when_changing_font_size = false
C.enable_scroll_bar = false
C.inactive_pane_hsb = { saturation = 1, brightness = 1 }
C.default_cursor_style = "SteadyBlock"
C.force_reverse_video_cursor = false

-- Colorscheme
-- ~ Monochromish {
-- C.color_scheme = "Grayscale (dark) (terminal.sexy)"
-- C.color_scheme = "Black Metal (Marduk) (base16)"
-- C.color_scheme = "VWbug (terminal.sexy)"
-- }
-- ~ Dim {
-- C.color_scheme = "Unsifted Wheat (terminal.sexy)"
-- C.color_scheme = "Twilight"
-- }
-- ~ Bright {
-- C.color_scheme = "Gruvbox Material (Gogh)"
C.color_scheme = "Shic (terminal.sexy)"
-- }
C.colors = {
  background = ayu.dragonInk1,
  cursor_fg = "#000000",
  cursor_bg = "#FFF779",
  tab_bar = {
    background = ayu.dragonInk1,
    active_tab = {
      bg_color = ayu.ansi[5],
      fg_color = ayu.dragonInk1,
      intensity = "Bold",
      underline = "None",
      italic = false,
      strikethrough = false,
    },
    inactive_tab = { bg_color = ayu.dragonInk1, fg_color = ayu.fg },
  },
}

-- Font
C.unicode_version = 14
C.font_size = 18
C.font_dirs = { "/usr/share/fonts/TTF/", "/usr/share/fonts/OTF/" }
C.font = W.font_with_fallback({
  { family = font_family, harfbuzz_features = font_features },
  { family = "Symbols Nerd Font Mono" },
  { family = "Font Awesome" },
})

-- Workspace
C.default_workspace = "home"

-- Performance
C.enable_wayland = true
C.animation_fps = 1
local gpus = W.gui.enumerate_gpus()
C.webgpu_preferred_adapter = gpus[1]
if gpus[2] ~= nil then
  C.front_end = "WebGpu"
else
  C.front_end = "OpenGL"
end
C.webgpu_force_fallback_adapter = true
C.scrollback_lines = 2000
C.audible_bell = "Disabled"
C.set_environment_variables = { CURRENT_TERM = "wezterm" }
C.check_for_updates = false

-- ~ Key ------------------------------------------------------------------- ~ --

C.leader = { key = "q", mods = "CTRL", timeout_milliseconds = 1000 }
C.keys = {
  map("phys:Space", act.ActivateCommandPalette),
  map("c", act.ActivateCopyMode),
  -- Pane
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
  -- Tab
  map("t", act.SpawnTab("CurrentPaneDomain")),
  map("[", act.ActivateTabRelative(-1)),
  map("]", act.ActivateTabRelative(1)),
  -- Move
  map("m", act.ActivateKeyTable({ name = "move_tab", one_shot = false })),
  { key = "{", mods = "LEADER|SHIFT", action = act.MoveTabRelative(-1) },
  { key = "}", mods = "LEADER|SHIFT", action = act.MoveTabRelative(1) },
  map("w", act.ShowLauncherArgs({ flags = "FUZZY|WORKSPACES" })),
}
C.key_tables = {
  resize_pane = {
    { key = "h", action = act.AdjustPaneSize({ "Left", 1 }) },
    { key = "j", action = act.AdjustPaneSize({ "Down", 1 }) },
    { key = "k", action = act.AdjustPaneSize({ "Up", 1 }) },
    { key = "l", action = act.AdjustPaneSize({ "Right", 1 }) },
    { key = "Escape", action = "PopKeyTable" },
    { key = "c", mods = "CTRL", action = "PopKeyTable" },
    { key = "Enter", action = "PopKeyTable" },
  },
  move_tab = {
    { key = "h", action = act.MoveTabRelative(-1) },
    { key = "j", action = act.MoveTabRelative(-1) },
    { key = "k", action = act.MoveTabRelative(1) },
    { key = "l", action = act.MoveTabRelative(1) },
    { key = "Escape", action = "PopKeyTable" },
    { key = "c", mods = "CTRL", action = "PopKeyTable" },
    { key = "Enter", action = "PopKeyTable" },
  },
}

return C
