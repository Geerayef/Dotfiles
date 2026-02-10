-- ~ Local ---------------------------------------------------------------- ~ --

local W = require("wezterm")
local G = W.GLOBAL
local C = W.config_builder()
local a = W.action
local nf = W.nerdfonts
local rb, rp = require("road").base, require("road").palette
local font = {
  fam = "Iosevka",
  fam_fb = "IosevkaTerm Nerd Font Mono",
  feat = { "calt=1", "clig=1", "liga=1", "dlig=1" },
  feat_fb = {},
}

G.icon_proc = {
  ["opam"] = nf.seti_ocaml,
  ["dune"] = nf.seti_ocaml,
  ["ocamlc"] = nf.seti_ocaml,
  ["cargo"] = nf.dev_rust,
  ["rustup"] = nf.dev_rust,
  ["lua"] = nf.seti_lua,
  ["fish"] = nf.fa_terminal,
  ["zsh"] = nf.dev_terminal,
  ["bash"] = nf.cod_terminal_bash,
  ["vim"] = nf.dev_vim,
  ["nvim"] = nf.linux_neovim,
  ["btop"] = nf.md_chart_donut_variant,
  ["git"] = nf.dev_git,
  ["gh"] = nf.dev_github_badge,
  ["presenterm"] = nf.fa_hashtag,
  ["moor"] = nf.seti_ruby,
  ["moar"] = nf.seti_ruby,
  ["sudo"] = nf.fa_hashtag,
}

-- ~ Key ------------------------------------------------------------------ ~ --

local function map(key, action)
  return { key = key, mods = "LEADER", action = action }
end

C.disable_default_key_bindings = true
C.leader = { key = "q", mods = "CTRL", timeout_milliseconds = 1000 }
C.keys = {
  map("phys:Space", a.ActivateCommandPalette),
  map("c", a.ActivateCopyMode),
  { key = "c", mods = "SHIFT|CTRL", action = a.CopyTo("Clipboard") },
  { key = "v", mods = "SHIFT|CTRL", action = a.PasteFrom("Clipboard") },
  { key = "=", mods = "CTRL", action = a.IncreaseFontSize },
  { key = "-", mods = "CTRL", action = a.DecreaseFontSize },
  { key = "0", mods = "CTRL", action = a.ResetFontSize },
  { key = "phys:Space", mods = "SHIFT|CTRL", action = a.QuickSelect },
  -- Pane
  map("h", a.ActivatePaneDirection("Left")),
  map("j", a.ActivatePaneDirection("Down")),
  map("k", a.ActivatePaneDirection("Up")),
  map("l", a.ActivatePaneDirection("Right")),
  map("s", a.SplitVertical({ domain = "CurrentPaneDomain" })),
  map("v", a.SplitHorizontal({ domain = "CurrentPaneDomain" })),
  map("x", a.CloseCurrentPane({ confirm = true })),
  map("z", a.TogglePaneZoomState),
  map("o", a.RotatePanes("Clockwise")),
  map("r", a.ActivateKeyTable({ name = "resize_pane", one_shot = false })),
  map("f", a.Search("CurrentSelectionOrEmptyString")),
  -- Tab
  map("t", a.SpawnTab("CurrentPaneDomain")),
  map("p", a.ActivateTabRelative(-1)),
  map("n", a.ActivateTabRelative(1)),
  -- Move
  map("m", a.ActivateKeyTable({ name = "move_tab", one_shot = false })),
  { key = "{", mods = "LEADER|SHIFT", action = a.MoveTabRelative(-1) },
  { key = "}", mods = "LEADER|SHIFT", action = a.MoveTabRelative(1) },
  map("w", a.ShowLauncherArgs({ flags = "FUZZY|WORKSPACES" })),
}

C.key_tables = {
  resize_pane = {
    { key = "h", action = a.AdjustPaneSize({ "Left", 1 }) },
    { key = "j", action = a.AdjustPaneSize({ "Down", 1 }) },
    { key = "k", action = a.AdjustPaneSize({ "Up", 1 }) },
    { key = "l", action = a.AdjustPaneSize({ "Right", 1 }) },
    { key = "Escape", action = a.PopKeyTable },
    { key = "Enter", action = a.PopKeyTable },
    { key = "c", mods = "CTRL", action = a.PopKeyTable },
  },
  move_tab = {
    { key = "h", action = a.MoveTabRelative(-1) },
    { key = "j", action = a.MoveTabRelative(-1) },
    { key = "k", action = a.MoveTabRelative(1) },
    { key = "l", action = a.MoveTabRelative(1) },
    { key = "Escape", action = a.PopKeyTable },
    { key = "Enter", action = a.PopKeyTable },
    { key = "c", mods = "CTRL", action = a.PopKeyTable },
  },
  search_mode = {
    { key = "n", mods = "CTRL", action = a.CopyMode("NextMatch") },
    { key = "p", mods = "CTRL", action = a.CopyMode("PriorMatch") },
    { key = "r", mods = "CTRL", action = a.CopyMode("CycleMatchType") },
    { key = "u", mods = "CTRL", action = a.CopyMode("ClearPattern") },
    { key = "p", mods = "ALT", action = a.CopyMode("PriorMatchPage") },
    { key = "n", mods = "ALT", action = a.CopyMode("NextMatchPage") },
    { key = "c", mods = "CTRL", action = a.CopyMode("Close") },
    { key = "Escape", mods = "NONE", action = a.CopyMode("Close") },
  },
}

-- ~ Statusbar ------------------------------------------------------------- ~ --

W.on("format-tab-title", function(tab)
  local program =
    string.gmatch(tab.active_pane.foreground_process_name, "([^/\\]+)$")
  return " " .. (G.icon_proc[program()] or nf.fa_gear) .. " "
end)

W.on("update-status", function(win, _)
  local s_clr = rb.emerald
  local s = win:active_workspace()
  if win:active_key_table() then
    s = win:active_key_table()
    if s == "move_tab" then
      s = "MOVE"
    elseif s == "resize_pane" then
      s = "SIZE"
    elseif s == "copy_mode" then
      s = "COPY"
    elseif s == "search_mode" then
      s = "FIND"
    end
    s_clr = rp.gunmetal[900]
  end
  if win:leader_is_active() then
    s = "LDR "
    s_clr = rb.rustyRed
  end
  win:set_left_status(W.format({
    { Text = "    " },
    { Foreground = { Color = s_clr } },
    { Text = nf.oct_table .. " " .. s },
    { Text = "    " },
    "ResetAttributes",
    { Text = "│ " },
  }))
  win:set_right_status(
    W.format({ { Text = nf.md_clock .. " " .. W.strftime("%H:%M") .. "    " } })
  )
end)

-- ~ Option ---------------------------------------------------------------- ~ --

-- Bar
C.tab_bar_at_bottom = true
C.tab_max_width = 128
C.use_fancy_tab_bar = false
C.show_new_tab_button_in_tab_bar = false
C.status_update_interval = 1000

-- Performance
C.enable_wayland = true
C.animation_fps = 1
C.max_fps = 24
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

-- Window
C.window_background_opacity = 1
C.window_decorations = "NONE"
C.window_frame = {}
C.window_padding = { left = "0%", right = "0%", top = "0%", bottom = "0%" }
C.use_resize_increments = true
C.adjust_window_size_when_changing_font_size = false
C.enable_scroll_bar = false
C.inactive_pane_hsb = { saturation = 1, brightness = 1 }
C.default_cursor_style = "SteadyBlock"
C.force_reverse_video_cursor = false

-- Colors
-- Batman | Gruvbox dark, hard (base16) | Digerati (terminal.sexy) | Mona Lisa (Gogh)
-- | Nature Suede (terminal.sexy)
-- Base: Dawn (terminal.sexy)
C.color_scheme = "Batman"
C.colors = {
  foreground = rb.mintCream,
  background = rb.dragonInk,
  cursor_fg = rb.dragonInk,
  cursor_bg = rb.lotusYellow,
  tab_bar = {
    background = rb.dragonInk,
    active_tab = {
      bg_color = rb.mintCream,
      fg_color = rb.dragonInk,
      intensity = "Bold",
    },
    inactive_tab = { bg_color = rb.dragonInk, fg_color = rb.mintCream },
  },
}

-- Font
-- stylua: ignore start
if string.match(font.fam_fb, "Term") ~= nil then
  font.feat_fb = { "calt=1", "clig=1", "liga=1", "dlig=1", "ss10", "cv01=2", "cv10=6", "cv26=12", "cv59=16", "cv85=6" }
elseif string.match(font.fam_fb, "Fira") ~= nil then
  font.feat_fb = { "zero", "calt=1", "clig=1", "liga=1", "dlig=1", "cv01", "cv02", "cv04", "cv08", "cv29", "cv30",
    "cv31", "ss01", "ss02", "ss05", "ss09" }
elseif string.match(font.fam_fb, "Victor") ~= nil then
  font.feat_fb = { "zero", "calt=1", "clig=1", "liga=1", "dlig=1", "cv01", "cv02", "cv04", "cv08", "cv29", "cv30",
    "cv31", "ss01", "ss02", "ss05", "ss09" }
elseif string.match(font.fam_fb, "Jet") ~= nil then
  font.feat_fb = { "calt=1", "clig=1", "liga=1", "dlig=1", "cv04", "cv07", "cv08", "cv17" }
end
-- stylua: ignore end
C.unicode_version = 14
C.font_size = 14
C.font_dirs = { "~/.local/share/fonts/", "/usr/share/fonts/TTF/" }
C.font = W.font_with_fallback({
  { family = font.fam, harfbuzz_features = font.feat, style = "Normal" },
  { family = font.fam_fb, harfbuzz_features = font.feat_fb },
  { family = "JetBrainsMono NFM" },
  { family = "Symbols Nerd Font Mono" },
  { family = "Font Awesome" },
})
C.line_height = 1.7
C.freetype_load_target = "Light"
C.freetype_render_target = "Normal"
C.freetype_load_flags = "DEFAULT|NO_AUTOHINT"
C.underline_position = "-0.2cell"

-- Workspace
C.default_workspace = "home"

return C
