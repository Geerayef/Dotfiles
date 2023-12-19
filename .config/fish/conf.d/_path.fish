# ~  $HOME paths

if test -d "$HOME/.bin"
    fish_add_path -a "$HOME/.bin"
end

if test -d "$HOME/.local/bin"
    fish_add_path -a "$HOME/.local/bin"
end

if test -d "$HOME/Software/Neovim/bin"
    fish_add_path -a "$HOME/Software/Neovim/bin"
end

if test -d "$HOME/.scripts"
    fish_add_path -a "$HOME/.scripts"
end

# ~  Developer Environment

# Cargo
if test -d "$HOME/.cargo"
    fish_add_path -a "$HOME/.cargo/bin"
end

# ghcup
if test -d "$HOME/.ghcup"
    fish_add_path -a "$HOME/.ghcup/bin"
end

# opam
if test -d "$HOME/.opam"
    and test -r "$HOME/.opam/opam-init/init.fish"
    fish_add_path -a "$HOME/.opam/default/bin"
    source "$HOME/.opam/opam-init/init.fish" >/dev/null 2>/dev/null
end

# pyenv
if test -e "$HOME/.pyenv"
    set -gx PYENV_ROOT "$HOME/.pyenv"
    command -v pyenv >/dev/null
    or fish_add_path -a "$PYENV_ROOT/bin"
    eval "$(pyenv init -)"
end