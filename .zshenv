# ~  Environment variables

# Locations
export XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
export ZDOTDIR=${ZDOTDIR:-${XDG_CONFIG_HOME}/zsh}
export FZF_BASE="/usr/bin/fzf"
export STARSHIP_CONFIG=${ZDOTDIR:-${XDG_CONFIG_HOME}/zsh}/starship.toml

# General settings
export HISTCONTROL=ignoreboth:erasedups
export TERM="alacritty"
export EDITOR="$(which nvim)"
export SUDO_EDITOR="$(which nvim)"
export VISUAL="$(which nvim)"
export RUFF_CACHE_DIR="$HOME/.cache/ruff"
export CONDA_AUTO_ACTIVATE_BASE=false

# Android
if [[ -d $HOME/Software/AndroidStudio ]] then
    export ANDROID_HOME="$HOME/Software/AndroidStudio/sdk"
    export ANDROID_USER_HOME="$HOME/Software/AndroidStudio/.android" 
fi

# -------------------------------------------------------------------------------- #

# ~ Path

[[ -d "$HOME/.bin" ]] && export PATH="$HOME/.bin:$PATH"

[[ -d "$HOME/.local/bin" ]] && export PATH="$HOME/.local/bin:$PATH"

[[ -d "$HOME/.local/share/nvim/mason/" ]] && export PATH="$HOME/.local/share/nvim/mason/bin:$PATH"

[[ -d "$HOME/Software/Neovim/bin" ]] && export PATH="$HOME/Software/Neovim/bin:$PATH"

[[ -d "$HOME/Scripts/" ]] && export PATH="$HOME/Scripts/:$PATH"

[[ -d "$HOME/linuxbrew/" ]] && export PATH="$HOME/linuxbrew/.linuxbrew/bin/" && eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"

[[ -d "$ANDROID_HOME" ]] && export PATH="$ANDROID_HOME/tools:$ANDROID_HOME/tools/bin:$ANDROID_HOME/platform-tools:$PATH"

[[ -d "$JAVA_HOME" ]] && export PATH="$JAVA_HOME:$PATH"
 
# -------------------------------------------------------------------------------- #

# ~  Developer Environment

# Rust: Cargo
[[ -d "$HOME/.cargo/" ]] && source "$HOME/.cargo/env"

# Haskell: ghcup
[[ -f "$HOME/.ghcup/env" ]] && source "$HOME/.ghcup/env"

# OCaml: opam
[[ ! -r "$HOME/.opam/opam-init/init.zsh" ]] || source "$HOME/.opam/opam-init/init.zsh"  > /dev/null 2> /dev/null

# Python: pyenv
if [[ -e $HOME/.pyenv ]] then
(
    export PYENV_ROOT="$HOME/.pyenv"
    command -v pyenv >/dev/null || export PATH="$PATH:$PYENV_ROOT/bin"
    eval "$(pyenv init -)"
)
fi

# -------------------------------------------------------------------------------- #

