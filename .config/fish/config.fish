set -U fish_greeting

if status is-interactive
    function fish_user_key_bindings
        fish_vi_key_bindings
    end
    # fish_config theme save "Catppuccin Macchiato"
    pyenv init - | source
    pyenv virtualenv-init - | source
    starship init fish | source
end

test -r '/home/tib/.opam/opam-init/init.fish' && source '/home/tib/.opam/opam-init/init.fish' >/dev/null 2>/dev/null; or true
