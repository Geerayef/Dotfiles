local wezterm = require("wezterm_alt")
local nf = wezterm.nerdfonts
local fmt = wezterm.format
local fmttimestr = wezterm.strftime
local Bar = {}

Bar.apply_to_config = function(config)
  local kanagawa = require("kanagawa")
  config.tab_max_width = 8
  config.enable_tab_bar = true
  config.use_fancy_tab_bar = false
  config.hide_tab_bar_if_only_one_tab = false
  config.tab_bar_at_bottom = true
  config.show_new_tab_button_in_tab_bar = false
  config.status_update_interval = 3000
  config.colors = {
    tab_bar = {
      background = kanagawa.split,
      active_tab = {
        bg_color = kanagawa.brights[3],
        fg_color = kanagawa.background,
        intensity = "Bold",
        underline = "None",
        italic = false,
        strikethrough = false,
      },
      inactive_tab = { bg_color = kanagawa.background, fg_color = kanagawa.foreground },
    },
  }
  wezterm.on("update-status", function(window, pane)
    local stat_color = kanagawa.indexed[16]
    local stat = window:active_workspace()
    if window:active_key_table() then
      stat = window:active_key_table()
      stat_color = kanagawa.brights[7]
    end
    if window:leader_is_active() then
      stat = "LDR "
      stat_color = kanagawa.ansi[5]
    end
    local time = fmttimestr("%H:%M")
    local battery_percentage = ""
    for _, b in ipairs(wezterm.battery_info()) do
      battery_percentage = string.format("%.0f%%", b.state_of_charge * 100)
    end
    window:set_left_status(fmt({
      { Text = "| " },
      { Foreground = { Color = stat_color } },
      { Text = nf.oct_table .. "  " .. stat },
      { Text = " |" },
    }))
    window:set_right_status(fmt({
      { Text = nf.md_clock .. "  " .. time },
      { Text = " | " },
      { Foreground = { Color = kanagawa.brights[4] } },
      { Text = nf.fa_battery_half .. "  " .. battery_percentage },
      "ResetAttributes",
      { Text = " |" },
    }))
  end)
end

return Bar
