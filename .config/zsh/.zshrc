# -------------------------------------------------------------------------------- #

# ~  Antidote setup

antidote_dir=${ZDOTDIR:-~/.config/zsh}/.antidote/
plugins=${ZDOTDIR:-~/.config/zsh}/.zsh_plugins.txt
static=${ZDOTDIR:-~/.config/zsh}/.zsh_plugins.zsh

[[ -e $antidote_dir ]] || git clone --depth=1 https://github.com/mattmc3/antidote.git $antidote_dir
[[ -f $plugins ]] || touch $plugins

fpath+=$antidote_dir
autoload -Uz $fpath[-1]/antidote

if [[ ! $static -nt $plugins ]]; then
    ( antidote bundle <$plugins >$static )
fi

source $static

unset antidote_dir plugins static

autoload -Uz promptinit && promptinit
# autoload -Uz compinit && compinit

# -------------------------------------------------------------------------------- #

# ~  Plugin settings

# Disable sort when completing `git checkout`
zstyle ':completion:*:git-checkout:*' sort false
# Set descriptions format to enable group support
zstyle ':completion:*:descriptions' format '[%d]'
# Set list-colors to enable filename colorizing
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
# Preview directory's content with exa when completing cd
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'exa -1 --color=always $realpath'
# Switch group using `,` and `.`
zstyle ':fzf-tab:*' switch-group ',' '.'
# Tmux style popup instead of default fzf
zstyle ':fzf-tab:*' fzf-command ftb-tmux-popup

# -------------------------------------------------------------------------------- #

# ~  Source

source $ZDOTDIR/functions.zsh
source $ZDOTDIR/aliases.zsh

# -------------------------------------------------------------------------------- #

# ~  General settings

bindkey -v

unsetopt SHARE_HISTORY
setopt GLOB_DOTS
setopt COMBINING_CHARS
DISABLE_AUTO_UPDATE="true"

# -------------------------------------------------------------------------------- #

# ~  Conda

if [[ -d "$HOME/miniconda3" ]] then
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
    __conda_setup="$('/home/tibor/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
    if [ $? -eq 0 ]; then
        eval "$__conda_setup"
    else
        if [ -f "/home/tibor/miniconda3/etc/profile.d/conda.sh" ]; then
            . "/home/tibor/miniconda3/etc/profile.d/conda.sh"
        else
            export PATH="/home/tibor/miniconda3/bin:$PATH"
        fi
    fi
    unset __conda_setup
# <<< conda initialize <<<

    currconda=$(check_conda)
    if [[ $currconda == "tf" ]]; then
        source $CONDA_PREFIX/etc/conda/activate.d/env_vars.sh
    fi
fi

# -------------------------------------------------------------------------------- #

# ~  Prompt

zle -N zle-line-init

eval "$(starship init zsh)"
