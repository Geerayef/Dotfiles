local wezterm = require("wezterm")
local Bar = {}

function Bar.apply_to_config(config)
  config.enable_tab_bar = true
  config.use_fancy_tab_bar = false
  config.hide_tab_bar_if_only_one_tab = false
  config.tab_bar_at_bottom = true
  config.show_new_tab_button_in_tab_bar = false
  config.status_update_interval = 1000
  config.colors = {
    tab_bar = {
      background = "#080C10",
      active_tab = {
        bg_color = "#010101",
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
  wezterm.on("update-status",
    function(window, pane)
      local stat = window:active_workspace()
      local stat_color = "#F7768E"
      if window:active_key_table() then
        stat = window:active_key_table()
        stat_color = "#7DCFFF"
      end
      if window:leader_is_active() then
        stat = "LDR "
        stat_color = "#BB9AF7"
      end

      local basename = function(s)
        return string.gsub(s, "(.*[/\\])(.*)", "%2")
      end

      local cwd = pane:get_current_working_dir()
      cwd = cwd and basename(cwd) or ""
      local cmd = pane:get_foreground_process_name()
      cmd = cmd and basename(cmd) or ""
      local time = wezterm.strftime("%H:%M")

      window:set_left_status(wezterm.format({
        { Foreground = { Color = stat_color } },
        { Text = "| " },
        { Text = wezterm.nerdfonts.oct_table .. "  " .. stat },
        { Text = " |" },
      }))

      window:set_right_status(wezterm.format({
        { Text = wezterm.nerdfonts.md_folder .. "  " .. cwd },
        { Text = " | " },
        { Foreground = { Color = "#e0af68" } },
        { Text = wezterm.nerdfonts.fa_code .. "  " .. cmd },
        "ResetAttributes",
        { Text = " | " },
        { Text = wezterm.nerdfonts.md_clock .. "  " .. time },
        { Text = " |" },
      }))
    end)
end

return Bar
