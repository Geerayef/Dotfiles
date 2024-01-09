local wezterm = require("wezterm")
local Bar = {}
local nf = wezterm.nerdfonts

function Bar.apply_to_config(config)
  local kanagawa = require("colors.kanagawa")
  config.enable_tab_bar = true
  config.use_fancy_tab_bar = false
  config.hide_tab_bar_if_only_one_tab = false
  config.tab_bar_at_bottom = true
  config.show_new_tab_button_in_tab_bar = false
  config.colors = {
    tab_bar = {
      background = kanagawa.background,
      active_tab = {
        bg_color = kanagawa.brights[1],
        fg_color = kanagawa.background,
        intensity = "Bold",
        underline = "None",
        italic = false,
        strikethrough = false,
      },
      inactive_tab = {
        bg_color = kanagawa.background,
        fg_color = kanagawa.foreground,
      },
    }
  }
  config.status_update_interval = 2000

  wezterm.on("update-status", function(window, pane)
    local basename = function(s) return string.gsub(s, "(.*[/\\])(.*)", "%2") end
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
    local cwd = true and basename(pane:get_current_working_dir()) or ""
    local cmd = true and basename(pane:get_foreground_process_name()) or ""
    local time = wezterm.strftime("%H:%M")
    local battery_percentage = ""
    for _, b in ipairs(wezterm.battery_info()) do battery_percentage = string.format('%.0f', b.state_of_charge * 100) end
    local battery_percent_value = tonumber(battery_percentage)
    local battery_icon = ""
    if battery_percent_value <= 25 then
      battery_icon = nf.fa_battery_empty
    elseif battery_percent_value > 25 and battery_percent_value <=50 then
      battery_icon = nf.fa_battery_quarter
    elseif battery_percent_value > 50 and battery_percent_value <= 75 then
      battery_icon = nf.fa_battery_half
    elseif battery_percent_value > 75 and battery_percent_value <= 100 then
      battery_icon = nf.fa_battery_full
    end
    window:set_left_status(wezterm.format({
      { Foreground = { Color = stat_color } },
      { Text = "| " },
      { Text = nf.oct_table .. "  " .. stat },
      { Text = " |" },
    }))
    window:set_right_status(wezterm.format({
      { Text = nf.md_folder .. "  " .. cwd },
      { Text = " | " },
      { Foreground = { Color = kanagawa.brights[4] } },
      { Text = nf.fa_code .. "  " .. cmd },
      "ResetAttributes",
      { Text = " | " },
      { Text = nf.md_clock .. "  " .. time },
      { Text = " | " },
      { Foreground = { Color = kanagawa.brights[4] } },
      { Text = battery_icon .. "  " .. battery_percentage .. "%" },
      "ResetAttributes",
      { Text = " |" }
    }))
  end)
end

return Bar
