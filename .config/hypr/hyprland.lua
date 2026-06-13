local road = __require("grnd.road").base

-- Environment
require("grnd.envs")

-- Start-up
require("grnd.stup")

-- Options
hl.config({
  general = {
    border_size = 1,
    col = { active_border = road.lotusYellow, inactive_border = road.jet },
    gaps_in = 4,
    gaps_out = 1,
    layout = "dwindle",
  },
  decoration = {
    rounding = 8,
    rounding_power = 1.0,
    blur = {
      xray = true,
      noise = 0.0117,
      contrast = 0.8916,
      brightness = 0.8172,
      vibrancy = 0.1696,
      vibrancy_darkness = 0.0,
    },
    shadow = { enabled = false },
    glow = { enabled = false },
  },
  input = {
    kb_model = "",
    kb_layout = "gb,us,rs",
    kb_variant = ",dvp,latinyz",
    kb_options = "ctrl:nocaps",
    kb_rules = "",
    follow_mouse = 2,
    sensitivity = 1.0,
    touchpad = { natural_scroll = true },
  },
  group = {
    col = {
      border_active = road.emerald,
      border_inactive = road.charcoal,
      border_locked_active = road.rustyRed,
      border_locked_inactive = road.paynesGray,
    },
    groupbar = {
      render_titles = false,
      gradients = false,
      scrolling = false,
      middle_click_close = false,
      col = {
        active = road.emerald,
        inactive = road.charcoal,
        locked_active = road.rustyRed,
        locked_inactive = road.paynesGray,
      },
    },
  },
  misc = {
    disable_hyprland_logo = true,
    disable_splash_rendering = true,
    focus_on_activate = true,
    font_family = "Iosevka",
    force_default_wallpaper = 0,
    key_press_enables_dpms = true,
    vrr = 0,
  },
  render = { new_render_scheduling = true },
  cursor = { inactive_timeout = 5 },
  ecosystem = { no_update_news = true, no_donation_nag = true },
})

-- Monitor
hl.monitor({ output = "eDP-1", mode = "1920x1080", position = "auto", scale = 1 })
hl.monitor({ output = "HDMI-A-1", mode = "preferred", position = "auto", scale = 1 })

-- Animation
do
  local a = hl.animation
  hl.curve("smooth", { type = "bezier", points = { { 0.59, 0.17 }, { 0.17, 0.87 } } })
  a({ leaf = "windows", enabled = true, speed = 1, bezier = "smooth" })
  a({ leaf = "windowsIn", enabled = true, speed = 1, bezier = "smooth", style = "popin 20%" })
  a({ leaf = "windowsOut", enabled = true, speed = 1, bezier = "smooth", style = "slide" })
  a({ leaf = "fade", enabled = true, speed = 1, bezier = "smooth" })
  a({ leaf = "workspaces", enabled = true, speed = 1, bezier = "smooth", style = "slidefade 10%" })
end

-- Keymap
require("grnd.keys")

-- Rule
-- workspace
hl.workspace_rule({ workspace = "w[tv1]s[false]", gaps_out = 0, gaps_in = 0 })
hl.workspace_rule({ workspace = "f[1]s[false]", gaps_out = 0, gaps_in = 0 })
-- window
hl.window_rule({ match = { float = false, workspace = "w[tv1]s[false]" }, border_size = 0 })
hl.window_rule({ match = { float = false, workspace = "w[tv1]s[false]" }, rounding = 0 })
hl.window_rule({ match = { float = false, workspace = "f[1]s[false]" }, border_size = 0 })
hl.window_rule({ match = { float = false, workspace = "f[1]s[false]" }, rounding = 0 })
