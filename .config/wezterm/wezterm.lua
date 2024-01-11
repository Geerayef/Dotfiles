local wezterm = require("wezterm")
local config = {}

if wezterm.config_builder then
    config = wezterm.config_builder()
end

-- ~ ----------------------------------------------------------------- ~ --

-- ~  Source

Bar = require("bar")
Bar.apply_to_config(config)

Keys = require("keymaps")
Keys.apply_to_config(config)

-- ~ ----------------------------------------------------------------- ~ --

-- ~  APPEARANCE

--  Window UI

config.window_background_opacity = 1
config.window_decorations = "RESIZE"
config.adjust_window_size_when_changing_font_size = false
config.use_resize_increments = true
config.enable_scroll_bar = false
config.window_padding = { left = "0pt", right = "0pt", top = "0pt", bottom = "0pt" }
config.inactive_pane_hsb = { saturation = 0.90, brightness = 0.5 }

--  Themes
-- ayu / Ayu Dark (Gogh) / Mirage (Gogh)
-- Tokyo Night (Gogh) / tokyonight / _night
-- terafox / carbonfox / Kasugano (terminal.sexy)
-- Trim Yer Beard (terminal.sexy) / VWbug (terminal.sexy)
-- astromouse (terminal.sexy)

config.color_scheme = "Ayu Dark (Gogh)"

-- Font

config.harfbuzz_features = { "zero", "ss01", "cv05" }
config.font_dirs = {
    "/usr/share/fonts/FiraCodeNF",
    "/usr/share/fonts/JetBrainsNF",
    "/usr/share/fonts/TTF",
}
config.font = wezterm.font_with_fallback {
    { family = "FiraCode Nerd Font Mono Medium" },
    { family = "JetBrainsMono Nerd Font Mono" },
    { family = "Hasklug Nerd Font Mono Medium" },
    { family = "Symbols Nerd Font" },
    { family = "Symbols Nerd Font Mono" },
}
config.font_size = 16
config.line_height = 1

-- Cursor

config.default_cursor_style = "SteadyBlock"
config.force_reverse_video_cursor = true

-- ~ ----------------------------------------------------------------- ~ --

-- ~  BEHAVIOUR

-- Workspace

config.default_workspace = "home"

-- General

config.enable_wayland = false
config.front_end = "OpenGL"
config.animation_fps = 1
config.scrollback_lines = 6000
config.audible_bell = "Disabled"
config.window_close_confirmation = "AlwaysPrompt"

return config
