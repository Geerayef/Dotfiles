# ~  Environment variables

# Locations
export XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
export XDG_STATE_HOME=${XDG_STATE_HOME:-$HOME/.local/state}
export XDG_DATA_HOME=${XDG_DATA_HOME:-$HOME/.local/share}
export XDG_DATA_DIRS="${XDG_STATE_HOME:-$HOME/.local/state}/nix/profiles/profile/share:$XDG_DATA_DIRS"
export FZF_BASE="/usr/bin/fzf"
export STARSHIP_CONFIG="$XDG_CONFIG_HOME/starship/starship.toml"
export ZDOTDIR=${ZDOTDIR:-${XDG_CONFIG_HOME}/zsh}
export BASHDOTDIR=${BASHDOTDIR:-${XDG_CONFIG_HOME}/bash}
export RUFF_CACHE_DIR="$HOME/.cache/ruff"
if [[ -d $HOME/Software/AndroidStudio ]] ; then
    export ANDROID_HOME="$HOME/Software/AndroidStudio/sdk"
    export ANDROID_USER_HOME="$HOME/Software/AndroidStudio/.android" 
fi

# General settings
neovim="$(which nvim)"
export TERM="wezterm"
export EDITOR="$neovim"
export SUDO_EDITOR="$neovim"
export VISUAL="$HOME/.scripts/nnn/edit_detached.fish"
export PAGER="nvimpager -- --noplugin -R -u $XDG_CONFIG_HOME/nvim/small.lua"
export MOZ_ENABLE_WAYLAND=1
# export HISTCONTROL=ignoreboth:erasedups
export CONDA_AUTO_ACTIVATE_BASE=false

# -------------------------------------------------------------------------------- #

# ~  Developer Environment

# Rust: Cargo
[[ -d "$HOME/.cargo/" ]] && source "$HOME/.cargo/env"

# Haskell: ghcup
[[ -f "$HOME/.ghcup/env" ]] && source "$HOME/.ghcup/env"

# OCaml: opam
[[ ! -r "$HOME/.opam/opam-init/init.sh" ]] || source "$HOME/.opam/opam-init/init.sh"  > /dev/null 2> /dev/null

# Python: pyenv
if [[ -e $HOME/.pyenv ]] ; then
(
    export PYENV_ROOT="$HOME/.pyenv"
    command -v pyenv >/dev/null || export PATH="$PATH:$PYENV_ROOT/bin"
    eval "$(pyenv init -)"
)
fi
