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
# set -g FZF_DEFAULT_COMMAND "fd"
# export FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS' --min-height=40 --height=100% --margin=1 --scroll-off=5 --border --preview=bat'
# set -g FZF_DEFAULT_OPTS $FZF_DEFAULT_OPTS ' --color=fg:#8ebaa4,fg+:#e0def4,info:#ccb1ed,marker:#575860'
# set -g FZF_DEFAULT_OPTS $FZF_DEFAULT_OPTS ' --color=pointer:#719cd6,prompt:#719cd6,hl+:#dbc074,header:#9d79d6'
