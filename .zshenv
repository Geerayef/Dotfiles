# -------------------------------------------------------------------------------- #

# ~  Environment variables

export XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
export ZDOTDIR=${ZDOTDIR:-${XDG_CONFIG_HOME}/zsh}

export EDITOR="/usr/bin/nvim"
export VISUAL="/usr/bin/nvim"

export FZF_BASE="/usr/bin/fzf"

export PATH="$PATH:$HOME/.local/bin"

export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PATH:$PYENV_ROOT/bin"
eval "$(pyenv init -)"
