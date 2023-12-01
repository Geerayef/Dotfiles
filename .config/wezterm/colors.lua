local Colors = {}

function Colors.apply_to_config(config)
  config.colors = {
    cursor_bg = "#528BFF",
    cursor_border = "#528BFF",
    cursor_fg = "black",
    tab_bar = {
      background = "#080C10",
      active_tab = {
        bg_color = "#000000",
        fg_color = "#B5BDC5",
        intensity = "Bold",
        underline = "None",
        italic = false,
        strikethrough = false,
      },
      inactive_tab = {
        bg_color = "#0F0F0F",
        fg_color = "#909090",
      },
    }
  }
end

return Colors
