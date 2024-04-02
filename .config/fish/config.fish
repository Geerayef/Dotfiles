set -U fish_greeting

if status --is-login
    if test (tty) = /dev/tty1
        Hyprland
    end
end

if status is-interactive
    function fish_user_key_bindings
        fish_vi_key_bindings
    end

    # ~  Source
    # pyenv init - | source

    # ~  Theme
    # fish_config theme save "Catppuccin Mocha"

    # ~  Prompt
    starship init fish | source
end
