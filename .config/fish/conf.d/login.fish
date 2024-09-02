if status --is-login
    if test (tty) = /dev/tty1
        dbus-run-session Hyprland
    end
end
