# ~  Locations

set -gx XDG_CACHE_HOME "$HOME/.cache"
set -gx XDG_CONFIG_HOME "$HOME/.config"
set -gx XDG_STATE_HOME "$HOME/.local/state"
set -gx XDG_DATA_HOME "$HOME/.local/share"
set -gx ZDOTDIR "$XDG_CONFIG_HOME/zsh"
set -gx BASHDOTDIR "$XDG_CONFIG_HOME/bash"
set -gx SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/gcr/ssh"
set -gx SSH_ENV "$HOME/.ssh/environment"
set -gx FZF_BASE /usr/bin/fzf
set -gx STARSHIP_CONFIG "$XDG_CONFIG_HOME/starship/starship.toml"
set -gx RUFF_CACHE_DIR "$HOME/.cache/ruff"
set -gx PNPM_HOME "$HOME/.local/share/pnpm"

# ~  General

set -gx TERM wezterm
set -gx EDITOR nvim
set -gx SUDO_EDITOR nvim
set -gx VISUAL edit.fish
set -gx PAGER most +s +u
set -gx MANPAGER nvim +Man!
set -gx FZF_COMPLETE 3
set -gx SYSTEM_PACKAGE_MANAGER pacman
set -gx MOZ_ENABLE_WAYLAND 1
set -gx CONDA_AUTO_ACTIVATE_BASE false
set -gx GTK_THEME Material
set -gx VK_DRIVER_FILES /usr/share/vulkan/icd.d/intel_icd.x86_64.json:/usr/share/vulkan/icd.d/nvidia_icd.json
