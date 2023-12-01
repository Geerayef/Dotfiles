# ~  Locations

set -gx XDG_CACHE_HOME "$HOME/.cache"
set -gx XDG_CONFIG_HOME "$HOME/.config"
set -gx XDG_STATE_HOME "$HOME/.local/state"
set -gx XDG_DATA_HOME "$HOME/.local/share"
set -gx XDG_DATA_DIRS "$HOME/.nix-profile/share" $XDG_DATA_DIRS
set -gx FZF_BASE "/usr/bin/fzf"
set -gx STARSHIP_CONFIG "$XDG_CONFIG_HOME/starship/starship.toml"
set -gx ZDOTDIR "$XDG_CONFIG_HOME/zsh"
set -gx BASHDOTDIR "$XDG_CONFIG_HOME/bash"
set -gx RUFF_CACHE_DIR "$HOME/.cache/ruff"

# ~  General

set -gx TERM "wezterm"
set -gx EDITOR $(which nvim)
set -gx SUDO_EDITOR $(which nvim)
set -gx VISUAL "$HOME/.scripts/nnn/edit_detached.fish"
set -gx PAGER "nvimpager -- nvim -u /home/tibor/.config/nvim/small.lua"
set -gx MANPAGER "nvim +Man!"
set -gx GDK_BACKEND "wayland"
set -gx CONDA_AUTO_ACTIVATE_BASE false
