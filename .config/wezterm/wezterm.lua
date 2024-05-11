local W = require("wezterm")
local G = W.GLOBAL
local act = W.action
local fmt = W.format
local nf = W.nerdfonts
local fmtime = W.strftime
local ayu = require("ayu")
local C = {}
if W.config_builder then C = W.config_builder() end

-- ~ -------------------------------------------------------------------------------- ~ --

-- ~  Globals

if G.tab_titles == nil then G.tab_titles = {} end

if G.process_icons == nil then
  G.process_icons = {
    ["fish"] = nf.fa_terminal,
    ["nvim"] = nf.linux_neovim,
    ["docker"] = nf.linux_docker,
    ["docker-compose"] = nf.linux_docker,
    ["psql"] = nf.dev_postgresql,
    ["make"] = nf.seti_makefile,
    ["vim"] = nf.dev_vim,
    ["go"] = nf.seti_go,
    ["zsh"] = nf.dev_terminal,
    ["bash"] = nf.cod_terminal_bash,
    ["btm"] = nf.md_chart_donut_variant,
    ["btop"] = nf.md_chart_donut_variant,
    ["cargo"] = nf.dev_rust,
    ["sudo"] = nf.fa_hashtag,
    ["lazydocker"] = nf.linux_docker,
    ["git"] = nf.dev_git,
    ["lua"] = nf.seti_lua,
    ["wget"] = nf.md_arrow_down_box,
    ["curl"] = nf.md_flattr,
    ["gh"] = nf.dev_github_badge,
    ["node"] = nf.dev_nodejs_small,
  }
end

if G.font_dirs == nil then
  G.font_dirs = {
    "/usr/share/fonts/TTF/",
    "/usr/share/fonts/OTF/",
    "/usr/share/fonts/TTF/FiraCodeNF/",
    "/usr/share/fonts/TTF/JetBrainsMonoNF/",
    "/usr/share/fonts/TTF/IosevkaNF/",
    "/usr/share/fonts/TTF/RobotoMonoNF/",
  }
end
if G.firacode_harfbuzz == nil then
  G.firacode_harfbuzz = {
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
end
if G.iosevka_harfbuzz == nil then
  G.iosevka_harfbuzz = { "calt=1", "clig=1", "liga=1", "dlig=1", "cv26=12", "cv85=6", "ss10" }
end
if G.jetbrainsmono_harfbuzz == nil then
  G.jetbrainsmono_harfbuzz = { "calt=1", "clig=1", "liga=1", "dlig=1", "cv04", "cv07", "cv08", "cv17" }
end
-- ~ -------------------------------------------------------------------------------- ~ --

-- ~  Functions

local function map(key, action) return { key = key, mods = "LEADER", action = action } end

-- local function tbl_merge(t1, t2)
--   for i = 1, #t2 do
--     t1[#t1 + 1] = t2[i]
--   end
--   return t1
-- end

-- local function basename(s) return string.gsub(s, "(.*[/\\])(.*)", "%2") end

-- local function dir_name_tail(cwd)
--   if cwd == nil then return "N/A" end
--   local path_stripped = cwd:match("^file:///(.+)") or cwd
--   local path = {}
--   for segment in string.gmatch(path_stripped, "[^/]+") do
--     table.insert(path, segment)
--   end
--   return path[#path]
-- end

-- local function get_cwd(tab)
--   local current_dir = tab.active_pane.current_working_dir or ""
--   return dir_name_tail(current_dir)
-- end

local function get_process(tab)
  local process = tab.active_pane.foreground_process_name:match("([^/\\]+)$")
  return G.process_icons[process] or string.format("%s", process)
end

-- ~ -------------------------------------------------------------------------------- ~ --

-- ~  Events

W.on("format-tab-title", function(tab) return " " .. get_process(tab) .. " " end)

W.on("update-status", function(window, _)
  local stat_color = ayu.indexed[16]
  local stat = window:active_workspace()
  if window:active_key_table() then
    stat = window:active_key_table()
    stat_color = ayu.brights[7]
  end
  if window:leader_is_active() then
    stat = "LDR "
    stat_color = ayu.ansi[5]
  end
  local time = fmtime("%H:%M")
  window:set_left_status(fmt({
    { Text = "| " },
    { Foreground = { Color = stat_color } },
    { Text = nf.oct_table .. "  " .. stat },
    { Text = " |" },
  }))
  window:set_right_status(fmt({
    { Text = "| " },
    { Foreground = { Color = ayu.brights[4] } },
    { Text = nf.md_clock .. "  " .. time },
    "ResetAttributes",
    { Text = " |" },
  }))
end)

-- ~ -------------------------------------------------------------------------------- ~ --

-- Bar
C.enable_tab_bar = true
C.tab_bar_at_bottom = true
C.use_fancy_tab_bar = false
C.hide_tab_bar_if_only_one_tab = false
C.show_new_tab_button_in_tab_bar = false
C.status_update_interval = 3000

-- Window
C.window_background_opacity = 1
C.window_decorations = "RESIZE"
C.window_padding = { left = "0pt", right = "0pt", top = "0pt", bottom = "0pt" }
C.use_resize_increments = true
C.adjust_window_size_when_changing_font_size = false
C.enable_scroll_bar = false
C.inactive_pane_hsb = { saturation = 0.9, brightness = 0.8 }
C.default_cursor_style = "SteadyBlock"
C.force_reverse_video_cursor = false

-- Colorscheme
C.color_scheme = "ayu"
C.colors = {
  cursor_fg = "#000000",
  tab_bar = {
    background = ayu.background,
    active_tab = {
      -- bg_color = ayu.brights[5],
      bg_color = "#9CABCA",
      fg_color = ayu.background,
      intensity = "Bold",
      underline = "None",
      italic = false,
      strikethrough = false,
    },
    inactive_tab = { bg_color = ayu.background, fg_color = ayu.foreground },
  },
}

-- Font
C.unicode_version = 14
C.bold_brightens_ansi_colors = true
C.allow_square_glyphs_to_overflow_width = "Always"
C.custom_block_glyphs = true
C.anti_alias_custom_block_glyphs = false
C.freetype_load_flags = "NO_HINTING|NO_BITMAP|NO_AUTOHINT"
C.freetype_load_target = "Light"
C.freetype_render_target = "HorizontalLcd"
C.harfbuzz_features = {}
C.font_dirs = G.font_dirs
C.font = W.font_with_fallback({
  { family = "IosevkaTerm Nerd Font Mono", harfbuzz_features = G.iosevka_harfbuzz },
  { family = "FiraCode Nerd Font Mono", harfbuzz_features = G.firacode_harfbuzz },
  { family = "JetBrainsMono Nerd Font Mono", harfbuzz_features = G.jetbrainsmono_harfbuzz },
  "RobotoMono Nerd Font Mono",
  "Symbols Nerd Font",
  "Symbols Nerd Font Mono",
})
C.font_size = 16
C.line_height = 1

-- Workspace
C.default_workspace = "home"

-- Performance
C.enable_wayland = false
C.animation_fps = 1
C.max_fps = 60
C.front_end = "OpenGL"
C.webgpu_force_fallback_adapter = false
C.scrollback_lines = 2000
C.audible_bell = "Disabled"
C.set_environment_variables = { CURRENT_TERM = "wezterm" }
C.check_for_updates = false

-- Keys
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
  -- map("n", act.ShowTabNavigator),
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

return C
