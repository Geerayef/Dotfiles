# ~  Locations

set -gx XDG_CACHE_HOME "$HOME/.cache"
set -gx XDG_CONFIG_HOME "$HOME/.config"
set -gx XDG_STATE_HOME "$HOME/.local/state"
set -gx XDG_DATA_HOME "$HOME/.local/share"
set -gx FZF_BASE /usr/bin/fzf
set -gx STARSHIP_CONFIG "$XDG_CONFIG_HOME/starship/starship.toml"
set -gx ZDOTDIR "$XDG_CONFIG_HOME/zsh"
set -gx BASHDOTDIR "$XDG_CONFIG_HOME/bash"
set -gx RUFF_CACHE_DIR "$HOME/.cache/ruff"
set -gx SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/gcr/ssh"

# ~  General

test -e "$(command -v nvim)" && set -l neovim $(which nvim)
set -gx TERM wezterm
set -gx EDITOR "$neovim"
set -gx SUDO_EDITOR "$neovim"
set -gx VISUAL "$HOME/.scripts/nnn/edit_detached.fish"
set -gx PAGER "nvimpager -p"
set -gx MANPAGER "nvim +Man!"
set -gx SYSTEM_PACKAGE_MANAGER pacman
set -gx MOZ_ENABLE_WAYLAND 1
set -gx CONDA_AUTO_ACTIVATE_BASE false
set -gx GTK_THEME Kripton
