function fish_greeting
end

if status --is-login
    if test (tty) = /dev/tty1
        Hyprland
    end
end

if status is-interactive
    function fish_user_key_bindings
        fish_vi_key_bindings
    end

    set -gx PNPM_HOME "/home/tibor/.local/share/pnpm"
    if not string match -q -- $PNPM_HOME $PATH
        set -gx PATH "$PNPM_HOME" $PATH
    end

    # ~  Source
    # pyenv init - | source

    # ~  Prompt
    starship init fish | source
end
