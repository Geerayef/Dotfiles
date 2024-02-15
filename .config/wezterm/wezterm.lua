local wezterm = require("wezterm")
local config = {}
if wezterm.config_builder then config = wezterm.config_builder() end

Bar = require("bar")
Bar.apply_to_config(config)
Keys = require("keymaps")
Keys.apply_to_config(config)

-- ~ -------------------------------------------------------------------------------- ~ --
-- ~  APPEARANCE

--  Window UI
config.window_background_opacity = 1
config.window_decorations = "RESIZE"
config.adjust_window_size_when_changing_font_size = false
config.use_resize_increments = true
config.enable_scroll_bar = false
config.window_padding = { left = "0pt", right = "0pt", top = "0pt", bottom = "0pt" }
config.inactive_pane_hsb = { saturation = 1, brightness = 0.8 }

--  Themes
-- ayu / Ayu Dark (Gogh) / Mirage (Gogh) / Nocturnal Winter / Tokyo Night (Gogh) / tokyonight
-- Kanagawa (Gogh) / terafox / carbonfox / Kasugano (terminal.sexy) / neobones_dark / Neutron (Gogh)
-- Trim Yer Beard (terminal.sexy) / VWbug (terminal.sexy) / N0Tch2K (Gogh)
-- astromouse (terminal.sexy)
config.color_scheme = "ayu"

-- Font
config.freetype_load_flags = "NO_HINTING"
config.harfbuzz_features = { "zero", "ss01", "cv05" }
config.font_dirs = { "/usr/share/fonts/FiraCodeNF", "/usr/share/fonts/JetBrainsNF", "/usr/share/fonts/TTF" }
config.font = wezterm.font_with_fallback({
  { family = "Iosevka Nerd Font Mono Regular" },
  { family = "JetBrainsMonoNL Nerd Font Mono Regular" },
  { family = "FiraCode Nerd Font Mono Regular" },
  { family = "Hasklug Nerd Font Mono Medium" },
  { family = "Symbols Nerd Font" },
  { family = "Symbols Nerd Font Mono" },
})
config.font_size = 14
config.line_height = 1

-- Cursor
config.default_cursor_style = "SteadyBlock"
config.force_reverse_video_cursor = false
config.colors.cursor_fg = "#000000"

-- ~ -------------------------------------------------------------------------------- ~ --
-- ~  BEHAVIOUR

-- Workspace
config.default_workspace = "home"

-- Performance
config.line_quad_cache_size = 1024 -- >= sum(#lines in panes in a tab) | < : Harms performance. [1024]
config.line_state_cache_size = 1024 -- As above. [1024]
config.line_to_ele_shape_cache_size = 1024 -- As above. [1024]
config.shape_cache_size = 1024 -- >= #(different attributed runs on the screen). [1024]

-- General
config.enable_wayland = false
-- config.front_end = "WebGpu"
-- config.webgpu_power_preference = "HighPerformance"
config.animation_fps = 1
config.max_fps = 60
config.scrollback_lines = 2000
config.audible_bell = "Disabled"
config.window_close_confirmation = "AlwaysPrompt"
config.set_environment_variables = { CURRENT_TERM = "wezterm" }

-- ~ -------------------------------------------------------------------------------- ~ --

return config
