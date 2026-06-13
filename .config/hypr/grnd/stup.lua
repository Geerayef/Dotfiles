local cmd = hl.dsp.exec_cmd

hl.on("hyprland.start", function()
  hl.dispatch(cmd("dbus-update-activation-environment --systemd --all"))
  hl.dispatch(
    cmd(
      "dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP HYPRLAND_INSTANCE_SIGNATURE"
    )
  )
  hl.dispatch(
    cmd(
      "systemctl --user import-environment DISPLAY WAYLAND_DISPLAY XDG_SESSION_DESKTOP XDG_CURRENT_DESKTOP QT_QPA_PLATFORMTHEME"
    )
  )
  hl.dispatch(cmd("systemctl --user start hyprpolkitagent"))
  hl.dispatch(cmd("mako"))
  hl.dispatch(cmd("hyprpaper"))
  hl.dispatch(cmd("hyprsunset"))
  hl.dispatch(cmd("hypridle"))
  hl.dispatch(cmd("wl-paste --type text --watch cliphist store"))
  hl.dispatch(cmd("wl-paste --type image --watch cliphist store"))
end)
