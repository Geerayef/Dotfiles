# ~  Environment variables

if [[ -n $BASHDOTDIR || -r $HOME/.config/bash/settings/env.bash ]]; then
    . ./.config/bash/settings/env.bash
else
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
    export HISTCONTROL=ignoreboth:erasedups
    [[ "$(command -v nvim)" ]] && neovim="$(which nvim)"
    export TERM="wezterm"
    export EDITOR="$neovim"
    export SUDO_EDITOR="$neovim"
    export VISUAL="$HOME/.scripts/nnn/edit_detached.fish"
    export PAGER="nvimpager -a"
    export MANPAGER="nvim +Man!"
    export SYSTEM_PACKAGE_MANAGER="pacman"
    export MOZ_ENABLE_WAYLAND=1
    export CONDA_AUTO_ACTIVATE_BASE=false

    # ~  Developer Environment

    # Rust: Cargo
    [[ -f "$HOME/.cargo/env" ]] && source "$HOME/.cargo/env"
    # Haskell: ghcup
    [[ -f "$HOME/.ghcup/env" ]] && source "$HOME/.ghcup/env"
    # OCaml: opam
    [[ ! -r "$HOME/.opam/opam-init/init.zsh" ]] || source "$HOME/.opam/opam-init/init.zsh" > /dev/null 2> /dev/null
    # Python: pyenv
    if [[ -e $HOME/.pyenv ]] ; then
    (
        export PYENV_ROOT="$HOME/.pyenv"
        command -v pyenv >/dev/null || export PATH="$PATH:$PYENV_ROOT/bin"
        eval "$(pyenv init -)"
    )
    fi
fi

# -------------------------------------------------------------------------------- #

# ~ Path

if [[ -n $BASHDOTDIR || -r $XDG_CONFIG_HOME/bash/settings/path.bash ]] ; then
    . ./.config/bash/settings/path.bash
else
    [[ -d "$HOME/.bin" ]] && export PATH="$HOME/.bin:$PATH"
    [[ -d "$HOME/.local/bin" ]] && export PATH="$HOME/.local/bin:$PATH"
    [[ -d "$HOME/.scripts/" ]] && export PATH="$HOME/.scripts/:$PATH"
    [[ -d "$HOME/Software/Neovim/bin" ]] && export PATH="$HOME/Software/Neovim/bin:$PATH"
    [[ -d "$HOME/linuxbrew/" ]] && export PATH="$HOME/linuxbrew/.linuxbrew/bin/" && eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
    [[ -d "$ANDROID_HOME" ]] && export PATH="$ANDROID_HOME/tools:$ANDROID_HOME/tools/bin:$ANDROID_HOME/platform-tools:$PATH"
    [[ -d "$JAVA_HOME" ]] && export PATH="$JAVA_HOME:$PATH"
fi
 
# -------------------------------------------------------------------------------- #

