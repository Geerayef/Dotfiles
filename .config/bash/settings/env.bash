# shellcheck shell=bash
# ~  Environment variables

# Locations
export XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
export XDG_STATE_HOME=${XDG_STATE_HOME:-$HOME/.local/state}
export XDG_DATA_HOME=${XDG_DATA_HOME:-$HOME/.local/share}
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh"
export ZDOTDIR=${ZDOTDIR:-${XDG_CONFIG_HOME}/zsh}
export BASHDOTDIR=${BASHDOTDIR:-${XDG_CONFIG_HOME}/bash}
export FZF_BASE="/usr/bin/fzf"
export RUFF_CACHE_DIR="$HOME/.cache/ruff"
export STARSHIP_CONFIG="$XDG_CONFIG_HOME/starship/starship.toml"

# General settings
[[ -n ${BASH_VERSION} || $SHELL == bash ]] && export HISTFILE="$BASHDOTDIR/history"
[[ -n ${ZSH_NAME} || $SHELL == zsh ]] && export HISTFILE="$ZDOTDIR/history"
export TERM="wezterm"
export EDITOR="nvim"
export SUDO_EDITOR="nvim"
export VISUAL="$HOME/.local/bin/scripts/edit.fish"
export PAGER="most"
export MANPAGER="nvim +Man!"
export SYSTEM_PACKAGE_MANAGER="pacman"
export MOZ_ENABLE_WAYLAND=1
export CONDA_AUTO_ACTIVATE_BASE=false
export FZF_DEFAULT_OPTS='--scheme=path --cycle --layout=reverse --border=sharp --scroll-off=5 --height=45% --preview-window=wrap,border-sharp --marker="*" --bind=ctrl-f:preview-half-page-down,ctrl-b:preview-half-page-up'

# -------------------------------------------------------------------------------- #

# ~  Developer Environment

# shellcheck disable=SC1091
{
  # Rust: Cargo
  [[ -f "$HOME/.cargo/env" ]] && source "$HOME/.cargo/env"
  # OCaml: opam
  if [[ $SHELL == "zsh" ]]; then
    [[ ! -r "$HOME/.opam/opam-init/init.zsh" ]] || source "$HOME/.opam/opam-init/init.zsh" >/dev/null 2>/dev/null
  elif [[ $SHELL == "bash" ]]; then
    [[ ! -r "$HOME/.opam/opam-init/init.sh" ]] || source "$HOME/.opam/opam-init/init.sh" >/dev/null 2>/dev/null
  else
    if [[ $SHELL == "fish" ]]; then echo '    [ WHAT??? ] Why is your {Z, BA}SH env getting sourced if your $SHELL=fish?'; fi
  fi
  # Python: pyenv
  if [[ -e $HOME/.pyenv ]]; then
    (
      export PYENV_ROOT="$HOME/.pyenv"
      command -v pyenv >/dev/null || export PATH="$PATH:$PYENV_ROOT/bin"
      eval "$(pyenv init -)"
    )
  fi
  # Haskell: ghcup
  [[ -f "$HOME/.ghcup/env" ]] && source "$HOME/.ghcup/env"
}
