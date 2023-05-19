# ~  Environment variables

# Locations
export XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
export ZDOTDIR=${ZDOTDIR:-${XDG_CONFIG_HOME}/zsh}
export MSW2_BASE=$HOME/Work/Projects/MSW2/Projects.SPS.MSW2/
export NEOVIM_HOME=$HOME/Software/Neovim/
export FZF_BASE="/usr/bin/fzf"

# General settings
export HISTCONTROL=ignoreboth:erasedups
export TERM="alacritty"
export EDITOR="$NEOVIM_HOME/bin/nvim"
export SUDO_EDITOR="nvim"
export VISUAL="$(which code)"
export BASHLS="**/*@(.sh|.zsh|.inc|.bash|.command)"
export FZF_CTRL_R_OPTS="
--preview 'echo {}' --preview-window up:3:hidden:wrap
--bind 'ctrl-/:toggle-preview'
--bind 'ctrl-y:execute-silent(echo -n {2..} | pbcopy)+abort'
--color header:italic"
export FZF_DEFAULT_OPTS="--extended --height 40% -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort $FZF_CTRL_R_OPTS --query=${(qqq)LBUFFER} +m"

# Android
if [[ -d $HOME/Software/AndroidStudio/ && -d $HOME/Software/AndroidStudio/ ]] then
    export ANDROID_HOME="$HOME/Software/AndroidStudio/Android/Sdk"
    export ANDROID_USER_HOME="$HOME/Software/AndroidStudio/" 
    export ANDROID_SDK_HOME="$HOME/Software/AndroidStudio"
fi

# -------------------------------------------------------------------------------- #

# ~ Path

[[ -d "$HOME/.bin" ]] && export PATH="$HOME/.bin:$PATH"

[[ -d "$HOME/.local/bin" ]] && export PATH="$HOME/.local/bin:$PATH"

[[ -d "$HOME/Software/Neovim/bin" ]] && export PATH="$HOME/Software/Neovim/bin:$PATH"

[[ -d "$ANDROID_HOME" ]] && export PATH="$ANDROID_HOME/tools:$ANDROID_HOME/tools/bin:$ANDROID_HOME/platform-tools:$PATH"

[[ -d "$HOME/Scripts/" ]] && export PATH="$HOME/Scripts/:$PATH"

[[ -d "$HOME/.local/share/JetBrains/" ]] && export PATH="$HOME/.local/share/JetBrains/Toolbox/scripts/:$PATH"

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

