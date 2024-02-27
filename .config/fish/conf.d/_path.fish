if test -d "$HOME/.bin"
    fish_add_path -a "$HOME/.bin"
end

if test -d "$HOME/.local/bin"
    fish_add_path -a "$HOME/.local/bin"
end

if test -d "$HOME/.local/bin/share/pnpm"
    fish_add_path -a "$HOME/.local/share/pnpm"
end

if test -d "$HOME/software/neovim/bin"
    fish_add_path -a "$HOME/software/neovim/bin"
end

if test -d "$HOME/.scripts"
    fish_add_path -a "$HOME/.scripts"
end

if test -d "$XDG_CONFIG_HOME/emacs/bin"
    fish_add_path -a "$XDG_CONFIG_HOME/emacs/bin"
end

# ~  Developer Environment

# Cargo
if test -d "$HOME/.cargo"
    fish_add_path -a "$HOME/.cargo/bin"
end

# pnpm
set -gx PNPM_HOME "/home/tibor/.local/share/pnpm"
if not string match -q -- $PNPM_HOME $PATH
    # set -gx PATH "$PNPM_HOME" $PATH
    fish_add_path -a "$PNPM_HOME"
end

# ghcup
if test -d "$HOME/.ghcup"
    fish_add_path -a "$HOME/.ghcup/bin"
end

# opam
if test -d "$HOME/.opam"
    not contains "$HOME/.opam/default/bin" $PATH
    and fish_add_path -a $HOME/.opam/default/bin
    # and source "$HOME/.opam/opam-init/init.fish"
end

# pyenv
if test -e "$HOME/.pyenv"
    set -gx PYENV_ROOT "$HOME/.pyenv"
    command -v pyenv >/dev/null
    or fish_add_path -a "$PYENV_ROOT/bin"
    eval "$(pyenv init -)"
end
