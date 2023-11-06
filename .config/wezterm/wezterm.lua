local wezterm = require("wezterm")
local config = {}

if wezterm.config_builder then
    config = wezterm.config_builder()
end

-- ~ ------------------------------------------------------- ~ --

-- ~ APPEARANCE ~ --

-- ~  Window UI

config.window_background_opacity = 0.9
config.window_decorations = "RESIZE"
config.adjust_window_size_when_changing_font_size = false
config.use_resize_increments = true
config.enable_scroll_bar = false
config.window_padding = {
  left   = '4pt',
  right  = '0pt',
  top    = '0pt',
  bottom = '0pt',
}
config.enable_tab_bar = true
config.use_fancy_tab_bar = false
config.hide_tab_bar_if_only_one_tab = true
config.tab_bar_at_bottom = true
config.show_new_tab_button_in_tab_bar = false
config.colors = {
  tab_bar = {
    background = '#000000',
    active_tab = {
      bg_color = '#010101',
      fg_color = '#ffffff',
      intensity = 'Bold',
      underline = 'None',
      italic = false,
      strikethrough = false,
    },
    inactive_tab = {
      bg_color = '#0f0f0f',
      fg_color = '#909090',
    },
  }
}

-- ~  Theme

-- Legend: [Optional] / variant

-- Blueish/Pale themes:
-- ayu / Ayu Dark (Gogh) / Mirage [(Gogh)]
-- Tokyo Night [(Gogh)] / Moon / Storm [(Gogh)]
-- tokyonight / -storm / _storm / _moon / _night
-- nightfox / duskfox / terafox / carbonfox / nordfox
-- Kasugano (terminal.sexy)

-- Red/Green -ish themes:
-- Trim Yer Beard (terminal.sexy) / VWbug (terminal.sexy)

-- Dark/Black themes:
-- astromouse (terminal.sexy) / Adventure / Ayu

local theme = "astromouse (terminal.sexy)"
config.color_scheme = theme

-- ~  Font

config.harfbuzz_features = { "zero" , "ss01", "cv05" }
config.font_dirs = {
    "/usr/share/fonts/JetBrainsNF/",
    "/usr/share/fonts/FiraCodeNF/",
}
config.font = wezterm.font_with_fallback {
    {
        family = "FiraCode Nerd Font Mono",
    },
    {
        family = "JetBrainsMono Nerd Font Propo",
    },
}
config.font_size = 18
config.line_height = 1

-- ~ ------------------------------------------------------- ~ --

-- ~ BEHAVIOUR ~ --

config.enable_wayland = true

-- ~  Cursor
config.default_cursor_style = "SteadyBlock"
config.force_reverse_video_cursor = true

return config
