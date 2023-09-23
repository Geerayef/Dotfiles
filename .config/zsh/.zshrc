# ~  Antidote setup

antidote_dir=${ZDOTDIR:-~/.config/zsh}/.antidote/
plugins=${ZDOTDIR:-~/.config/zsh}/.zsh_plugins.txt
static=${ZDOTDIR:-~/.config/zsh}/.zsh_plugins.zsh
fpath+=$antidote_dir

[[ -e $antidote_dir ]] || git clone --depth=1 https://github.com/mattmc3/antidote.git $antidote_dir
[[ -f $plugins ]] || touch $plugins

autoload -Uz $fpath[-1]/antidote

if [[ ! $static -nt $plugins ]]; then
    ( antidote bundle <$plugins >$static )
fi

source $static

unset antidote_dir plugins static

# -------------------------------------------------------------------------------- #

# ~  General settings

bindkey -v

unsetopt SHARE_HISTORY
setopt GLOB_DOTS
setopt COMBINING_CHARS

autoload -Uz promptinit && promptinit
# autoload -Uz compinit && compinit # Done by belak/zsh-utils/completions

# -------------------------------------------------------------------------------- #

# ~  Source

source $ZDOTDIR/functions.zsh
source $ZDOTDIR/aliases.zsh
source $ZDOTDIR/env.zsh
fpath+=${ZDOTDIR:-~/.config/zsh}/completions/

# -------------------------------------------------------------------------------- #

# ~  Plugin settings

zstyle ':completion:*:git-checkout:*' sort false
zstyle ':completion:*:descriptions' format '[%d]'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'exa -1 --color=always $realpath'
zstyle ':fzf-tab:*' switch-group ',' '.'
zstyle ':fzf-tab:*' fzf-command ftb-tmux-popup
zstyle ':zsh-utils:plugins:history' use-xdg-basedirs

# -------------------------------------------------------------------------------- #

# ~  Conda

if [[ -d "$HOME/miniconda3" ]] then
# >>> conda initialize >>>
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
