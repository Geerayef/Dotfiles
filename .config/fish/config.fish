set -U fish_greeting

if status is-login
    if test -r "$SSH_ENV"
        . $SSH_ENV >/dev/null
    else
        fissh gh >/dev/null 2>/dev/null
        if test $status -ne 0
            notify ERROR "Failed to start SSH agent with default key."
        end
    end
end

if status is-interactive
    function fish_user_key_bindings
        fish_default_key_bindings
    end
    if test -f /home/tib/miniconda3/bin/conda
        eval /home/tib/miniconda3/bin/conda "shell.fish" hook $argv | source
    else
        if test -f "/home/tib/miniconda3/etc/fish/conf.d/conda.fish"
            . "/home/tib/miniconda3/etc/fish/conf.d/conda.fish"
        else
            set -x PATH /home/tib/miniconda3/bin $PATH
        end
    end
    test -r "$HOME/.opam/opam-init/init.fish" && source "$HOME/.opam/opam-init/init.fish" >/dev/null 2>/dev/null; or true
    pyenv init - | source
    pyenv virtualenv-init - | source
    starship init fish | source
end
