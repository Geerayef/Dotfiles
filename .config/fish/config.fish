set -U fish_greeting

test -r '/home/tib/.opam/opam-init/init.fish' && source '/home/tib/.opam/opam-init/init.fish' >/dev/null 2>/dev/null; or true

if test -f /home/tib/miniconda3/bin/conda
    eval /home/tib/miniconda3/bin/conda "shell.fish" "hook" $argv | source
else
    if test -f "/home/tib/miniconda3/etc/fish/conf.d/conda.fish"
        . "/home/tib/miniconda3/etc/fish/conf.d/conda.fish"
    else
        set -x PATH "/home/tib/miniconda3/bin" $PATH
    end
end

if status is-interactive
    function fish_user_key_bindings
        fish_default_key_bindings
        # fish_default_key_bindings -M insert
        # fish_vi_key_bindings --no-erase insert
    end
    # fish_config theme save "Catppuccin Macchiato"
    pyenv init - | source
    pyenv virtualenv-init - | source
    starship init fish | source
end
