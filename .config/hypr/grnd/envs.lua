-- ~ Environment variables

-- Nvidia
hl.env("GBM_BACKEND", "nvidia-drm")
hl.env("__GLX_VENDOR_LIBRARY_NAME", "nvidia")
hl.env("LIBVA_DRIVER_NAME", "nvidia")
hl.env("WLR_NO_HARDWARE_CURSORS", "1")
-- Accesibility
hl.env("NO_AT_BRIDGE", "1")
-- GDK
hl.env("GDK_BACKEND", "wayland,x11")
-- XDG
hl.env("XDG_SESSION_TYPE", "wayland")
hl.env("XDG_SESSION_DESKTOP", "Hyprland")
hl.env("XDG_CURRENT_DESKTOP", "Hyprland")
-- QT
hl.env("QT_QPA_PLATFORM", "wayland;xcb")
hl.env("QT_WAYLAND_DISABLE_WINDOWDECORATION", "1")
hl.env("QT_QPA_PLATFORMTHEME", "qt5ct")
hl.env("QT_AUTO_SCREEN_SCALE_FACTOR", "1")
-- SDL
hl.env("SDL_VIDEODRIVER", "wayland")
-- Looks
hl.env("HYPRCURSOR_THEME", "BreezeX")
hl.env("HYPRCURSOR_SIZE", "32")
hl.env("XCURSOR_THEME", "BreezeX")
hl.env("XCURSOR_SIZE", "32")
