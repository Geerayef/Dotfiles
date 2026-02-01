# ~  Locations

set -gx XDG_CACHE_HOME "$HOME/.cache"
set -gx XDG_CONFIG_HOME "$HOME/.config"
set -gx XDG_STATE_HOME "$HOME/.local/state"
set -gx XDG_DATA_HOME "$HOME/.local/share"
set -gx ZDOTDIR "$XDG_CONFIG_HOME/zsh"
set -gx BASHDOTDIR "$XDG_CONFIG_HOME/bash"
set -gx SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh"
set -gx FZF_BASE (which fzf)
set -gx STARSHIP_CONFIG "$XDG_CONFIG_HOME/starship/starship.toml"
set -gx RUFF_CACHE_DIR "$XDG_CACHE_HOME/ruff"
set -gx PNPM_HOME "$XDG_DATA_HOME/pnpm"

# ~  General
set -gx TERM wezterm
set -gx EDITOR nvim
set -gx SUDO_EDITOR nvim
set -gx VISUAL edit.fish
set -gx PAGER moor
set -gx MANPAGER nvim +Man!
set -gx FZF_COMPLETE 3
set -gx SYSTEM_PACKAGE_MANAGER pacman
set -gx MOZ_ENABLE_WAYLAND 1
set -gx CONDA_AUTO_ACTIVATE_BASE false
set -gx GTK_THEME Material
set -gx VK_DRIVER_FILES /usr/share/vulkan/icd.d/intel_icd.x86_64.json:/usr/share/vulkan/icd.d/nvidia_icd.json
set -gx MOAR "-colors=256 -wrap -style=rose-pine -no-statusbar -no-linenumbers=false"
set -gx BUN_INSTALL "$HOME/.local"
set -gx LUA_PATH "$HOME/.local/lib/lua/?.lua;$HOME/.local/lib/lua/?/?.lua;$HOME/.local/lib/lua/?/init.lua;$HOME/.local/lib/lua/?/lua/?.lua;$HOME/.local/lib/lua/?/lua/init.lua;;"
# Haskell: GHCUP
set -gx GHCUP_USE_XDG_DIRS 1
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
set -gx PATH $HOME/.cabal/bin $PATH /home/tib/.local/bin
