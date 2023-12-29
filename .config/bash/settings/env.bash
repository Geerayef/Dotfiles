# ~  Environment variables

# Locations
export XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
export XDG_STATE_HOME=${XDG_STATE_HOME:-$HOME/.local/state}
export XDG_DATA_HOME=${XDG_DATA_HOME:-$HOME/.local/share}
export FZF_BASE="/usr/bin/fzf"
export STARSHIP_CONFIG="$XDG_CONFIG_HOME/starship/starship.toml"
export ZDOTDIR=${ZDOTDIR:-${XDG_CONFIG_HOME}/zsh}
export BASHDOTDIR=${BASHDOTDIR:-${XDG_CONFIG_HOME}/bash}
export RUFF_CACHE_DIR="$HOME/.cache/ruff"
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gcr/ssh"

# General settings
[[ -n "${BASH_VERSION}" || "$SHELL" = bash ]] && export HISTFILE="$BASHDOTDIR/history"
[[ -n "${ZSH_NAME}" || "$SHELL" = zsh ]] && export HISTFILE="$ZDOTDIR/history"
[[ -e "$(command -v nvim)" ]] && neovim="$(which nvim)"
export TERM="wezterm"
export EDITOR="$neovim"
export SUDO_EDITOR="$neovim"
export VISUAL="$HOME/.scripts/nnn/edit_detached.fish"
export PAGER="nvimpager -p"
export MANPAGER="nvim +Man!"
export SYSTEM_PACKAGE_MANAGER="pacman"
export MOZ_ENABLE_WAYLAND=1
export CONDA_AUTO_ACTIVATE_BASE=false
export FZF_DEFAULT_OPTS='--scheme=path --cycle --layout=reverse --border --scroll-off=5 --height=45% --preview-window=wrap --marker="*"'

# -------------------------------------------------------------------------------- #

# ~  Developer Environment

# shellcheck disable=SC1091
{
    # Rust: Cargo
    [[ -f "$HOME/.cargo/env" ]] && source "$HOME/.cargo/env"
    # Haskell: ghcup
    [[ -f "$HOME/.ghcup/env" ]] && source "$HOME/.ghcup/env"
    # OCaml: opam
    if [[ "$SHELL" = "zsh" ]] ; then
        [[ ! -r "$HOME/.opam/opam-init/init.zsh" ]] || source "$HOME/.opam/opam-init/init.zsh" > /dev/null 2> /dev/null
    elif [[ "$SHELL" = "bash" ]] ; then
        [[ ! -r "$HOME/.opam/opam-init/init.sh" ]] || source "$HOME/.opam/opam-init/init.sh" > /dev/null 2> /dev/null
    else
        if [[ "$SHELL" = "fish" ]] ; then echo "~~~~~ Why is your {Z, BA}SH env getting sourced if your SHELL = fish?"; fi
    fi
    # Python: pyenv
    if [[ -e $HOME/.pyenv ]] ; then
        (
        export PYENV_ROOT="$HOME/.pyenv"
        command -v pyenv >/dev/null || export PATH="$PATH:$PYENV_ROOT/bin"
        eval "$(pyenv init -)"
    )
    fi
}
