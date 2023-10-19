# ~  Locations

set -gx XDG_CACHE_HOME "$HOME/.cache"
set -gx XDG_CONFIG_HOME "$HOME/.config"
set -gx XDG_DATA_HOME "$HOME/.local/share"
set -gx XDG_STATE_HOME "$HOME/.local/state"
set -gx ZDOTDIR "$XDG_CONFIG_HOME/zsh"
set -gx FZF_BASE "/usr/bin/fzf"
set -gx STARSHIP_CONFIG "$XDG_CONFIG_HOME/starship/starship.toml"
set -gx RUFF_CACHE_DIR "$HOME/.cache/ruff"

# ~  General

set -x TERM "alacritty"
set -x EDITOR "$(which nvim)"
set -x SUDO_EDITOR "$(which nvim)"
set -x VISUAL "$(which nvim)"
set -x CONDA_AUTO_ACTIVATE_BASE false
set -x MANPAGER "nvim +Man!"
set -gx GDK_BACKEND wayland
