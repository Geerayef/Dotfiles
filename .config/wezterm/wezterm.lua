local wezterm = require("wezterm")
local config = {}

if wezterm.config_builder then
    config = wezterm.config_builder()
end

-- ~ ------------------------------------------------------- ~ --

-- ~  Source

Bar = require("bar")
Bar.apply_to_config(config)

Keys = require("keymaps")
Keys.apply_to_config(config)

Colors = require("colors")
Colors.apply_to_config(config)

-- ~ ------------------------------------------------------- ~ --

-- ~  APPEARANCE

--  Window UI

config.window_background_opacity = 0.9
config.window_decorations = "RESIZE"
config.adjust_window_size_when_changing_font_size = false
config.use_resize_increments = true
config.enable_scroll_bar = false
config.window_padding = {
  left   = '0pt',
  right  = '0pt',
  top    = '0pt',
  bottom = '0pt',
}
config.inactive_pane_hsb = {
  saturation = 0.25,
  brightness = 0.5
}

--  Themes: [Optional] / variant

-- PALE
-- ayu / Ayu Dark (Gogh) / Mirage [(Gogh)]
-- Tokyo Night [(Gogh)] / Moon / Storm [(Gogh)]
-- tokyonight / -storm / _storm / _moon / _night
-- nightfox / duskfox / terafox / carbonfox / nordfox
-- Kasugano (terminal.sexy)
-- SEPIA
-- Trim Yer Beard (terminal.sexy) / VWbug (terminal.sexy)
-- BLACK
-- astromouse (terminal.sexy) / Adventure / Ayu

local theme = "Ayu Dark (Gogh)"
config.color_scheme = theme

--  Font

config.harfbuzz_features = { "zero" , "ss01", "cv05" }
config.font_dirs = {
    "/usr/share/fonts/JetBrainsNF/",
    "/usr/share/fonts/FiraCodeNF/",
    "/usr/share/fonts/gnu-free/"
}
config.font = wezterm.font_with_fallback {
  "FiraCode Nerd Font Mono",
  "FiraCode Nerd Font Propo",
  "JetBrains Mono Nerd Font Mono",
  "JetBrains Mono Nerd Font Propo"
}
config.font_size = 18
config.line_height = 1

--  Cursor

config.default_cursor_style = "SteadyBlock"
config.force_reverse_video_cursor = false

-- ~ ------------------------------------------------------- ~ --

-- ~ BEHAVIOUR

--  Workspace

config.default_workspace = "home"

--  General

config.enable_wayland = true
config.animation_fps = 1
config.scrollback_lines = 3000
config.audible_bell = "Disabled"
config.window_close_confirmation = "AlwaysPrompt"

return config
