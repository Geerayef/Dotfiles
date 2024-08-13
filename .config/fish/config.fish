set -U fish_greeting

if status --is-login
    if test (tty) = /dev/tty1
        exec Hyprland
    end
end

if status is-interactive
    function fish_user_key_bindings
        fish_vi_key_bindings
    end
    # fish_config theme save "Catppuccin Macchiato"
    pyenv init - | source
    pyenv virtualenv-init - | source
    starship init fish | source
end


# BEGIN opam configuration
# This is useful if you're using opam as it adds:
#   - the correct directories to the PATH
#   - auto-completion for the opam binary
# This section can be safely removed at any time if needed.
test -r '/home/tib/.opam/opam-init/init.fish' && source '/home/tib/.opam/opam-init/init.fish' > /dev/null 2> /dev/null; or true
# END opam configuration
