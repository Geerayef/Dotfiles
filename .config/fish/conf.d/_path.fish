if test -d "$HOME/.local/bin"
    fish_add_path -a --path "$HOME/.local/bin/scripts"
end

if test -d "$HOME/.local/share/nvim/mason/bin"
    fish_add_path -a --path "$HOME/.local/share/nvim/mason/bin"
end

# ~  Developer Environment

# opam
if test -d "$HOME/.opam"
    not contains "$HOME/.opam/default/bin" $PATH
    and fish_add_path -a --path "$HOME/.opam/default/bin"
    # and source "$HOME/.opam/opam-init/init.fish"
end

# Cargo
if test -d "$HOME/.cargo"
    fish_add_path -a --path "$HOME/.cargo/bin"
end

# pnpm
if not string match -q -- $PNPM_HOME $PATH
    fish_add_path -a --path "$PNPM_HOME"
end

# ghcup
if test -d "$HOME/.ghcup"
    fish_add_path -a --path "$HOME/.ghcup/bin"
end

# pyenv
if test -e "$HOME/.pyenv"
    set -gx PYENV_ROOT "$HOME/.pyenv"
    command -v pyenv >/dev/null
    or fish_add_path -a --path "$PYENV_ROOT/bin"
    eval "$(pyenv init -)"
end

# Tex
if test -d /usr/local/texlive
    fish_add_path -p --path /usr/local/texlive/2024/bin/x86_64-linux
else
    if test -d "$HOME/.local/texlive2024"
        fish_add_path -p --path "$HOME/.local/texlive2024/bin/x86_64-linux"
    end
end
