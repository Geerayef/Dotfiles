# -------------------------------------------------------------------------------- #

# ~  Environment variables

# Locations
export XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
export ZDOTDIR=${ZDOTDIR:-${XDG_CONFIG_HOME}/zsh}

# General options
export EDITOR="/usr/bin/nvim"
export VISUAL="/usr/bin/nvim"
export HISTCONTROL=ignoreboth:erasedups
export FZF_BASE="/usr/bin/fzf"

# ~ Paths

if [[ -d "$HOME/.bin" ]]
  then PATH="$HOME/.bin:$PATH"
fi

if [[ -d "$HOME/.local/bin" ]]
  then PATH="$HOME/.local/bin:$PATH"
fi

if [[ -e $HOME/.pyenv ]] then
(
    export PYENV_ROOT="$HOME/.pyenv"
    command -v pyenv >/dev/null || export PATH="$PATH:$PYENV_ROOT/bin"
    eval "$(pyenv init -)"
)
echo "pyenv is not installed.
"
fi