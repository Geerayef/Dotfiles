local Colors = {}

function Colors.apply_to_config(config)
  local kanagawa = require("colors.kanagawa")
  config.colors = kanagawa.colors
  config.colors = {
    cursor_bg = "#528BFF",
    cursor_border = "#528BFF",
    cursor_fg = "black",
    tab_bar = {
      background = kanagawa.colors.background,
      active_tab = {
        bg_color = kanagawa.colors.brights[1],
        fg_color = kanagawa.colors.background,
        intensity = "Bold",
        underline = "None",
        italic = false,
        strikethrough = false,
      },
      inactive_tab = {
        -- "#0F0F0F", 
        -- "#909090", 
        bg_color = kanagawa.colors.background,
        fg_color = kanagawa.colors.foreground,
      },
    }
  }
end

return Colors
