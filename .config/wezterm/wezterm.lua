local wezterm = require "wezterm"
local config = {}

if wezterm.config_builder then
    config = wezterm.config_builder()
end

------------------------------------------------------------

-- ~  Window UI

config.harfbuzz_features = {"zero" , "ss01", "cv05"}

-- Tab
config.enable_tab_bar = true
config.hide_tab_bar_if_only_one_tab = true
config.tab_bar_at_bottom = true

-- Window
config.window_decorations = "NONE"
config.window_background_opacity = 0.92
config.adjust_window_size_when_changing_font_size = false

------------------------------------------------------------

-- Color
-- Legend: [Optional] / variant

-- Ayu:
-- ayu
-- Ayu / Dark (Gogh) / Mirage [(Gogh)]
-- folke:
-- Tokyo Night [(Gogh)] / Moon / Storm [(Gogh)]
-- tokyonight / -storm / _storm / _moon / _night
config.color_scheme = "Ayu Dark (Gogh)"
config.font = wezterm.font_with_fallback {
    {
        family = "Fira Code NerdFontMono",
    },
    "Jetbrains Mono"
}

-- Font
config.font_size = 16

-- Cursor
config.cursor_blink_rate = 0
config.default_cursor_style = "SteadyBlock"
config.cursor_thickness = "9pt"

return config
