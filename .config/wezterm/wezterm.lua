local wezterm = require("wezterm")
local config = {}

if wezterm.config_builder then
    config = wezterm.config_builder()
end

------------------------------------------------------------

-- ~ APPEARANCE ~ --

-----------------------------------------------------------

-- ~  Window UI

config.enable_tab_bar = true
config.hide_tab_bar_if_only_one_tab = true
config.tab_bar_at_bottom = true
config.window_padding = {
  left = 8,
  right = 8,
  top = 16,
  bottom = 0,
}

config.window_background_opacity = 0.925
config.window_decorations = "NONE"
config.adjust_window_size_when_changing_font_size = false

-- ~  Theme

-- Legend: [Optional] / variant
-- ayu / Ayu / Dark (Gogh) / Mirage [(Gogh)]
-- Tokyo Night [(Gogh)] / Moon / Storm [(Gogh)]
-- tokyonight / -storm / _storm / _moon / _night
-- Sublette
-- Rouge 2
-- Palenight (Gogh)
-- nord / fox / Nord (Gogh)
-- Kasugano (terminal.sexy)
-- Trim Yer Beard (terminal.sexy)
-- VWbug (terminal.sexy)

local theme = "Rouge 2"
config.color_scheme = theme

-- ~  Font

config.harfbuzz_features = {"zero" , "ss01", "cv05"}
config.font_dirs = { "/usr/share/fonts/FiraCodeNF" }
config.font = wezterm.font_with_fallback {
    {
        family = "FiraCodeNerdFontMono",
    },
    "JetBrains Mono",
}
config.font_size = 16
config.line_height = 1.1

------------------------------------------------------------

-- # BEHAVIOUR # --

------------------------------------------------------------

-- ~  Cursor
config.default_cursor_style = "SteadyBlock"
config.force_reverse_video_cursor = true

return config

