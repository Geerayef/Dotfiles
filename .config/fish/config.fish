set -U fish_greeting

if status is-interactive
    function fish_user_key_bindings
        fish_default_key_bindings
    end
    test -r "$HOME/.opam/opam-init/init.fish" && source "$HOME/.opam/opam-init/init.fish" >/dev/null 2>/dev/null
    pyenv init - | source
    pyenv virtualenv-init - | source
    starship init fish | source
end
