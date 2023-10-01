# ~  Environment variables

# Locations
export XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
export XDG_DATA_HOME=${XDG_DATA_HOME:-$HOME/.local/share}
export XDG_STATE_HOME=${XDG_STATE_HOME:-$HOME/.local/state}
export ZDOTDIR=${ZDOTDIR:-${XDG_CONFIG_HOME}/zsh}
export FZF_BASE="/usr/bin/fzf"
export STARSHIP_CONFIG="$XDG_CONFIG_HOME/starship/starship.toml"
export RUFF_CACHE_DIR="$HOME/.cache/ruff"
if [[ -d $HOME/Software/AndroidStudio ]] then
    export ANDROID_HOME="$HOME/Software/AndroidStudio/sdk"
    export ANDROID_USER_HOME="$HOME/Software/AndroidStudio/.android" 
fi

# General settings
export HISTCONTROL=ignoreboth:erasedups
export TERM="alacritty"
export EDITOR="$(which nvim)"
export SUDO_EDITOR="$(which nvim)"
export VISUAL="$(which nvim)"
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
if [[ -e $HOME/.pyenv ]] then
(
    export PYENV_ROOT="$HOME/.pyenv"
    command -v pyenv >/dev/null || export PATH="$PATH:$PYENV_ROOT/bin"
    eval "$(pyenv init -)"
)
fi
