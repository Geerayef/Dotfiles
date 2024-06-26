# ~  fpath

fpath+=${ZDOTDIR:-~/.config/zsh}/completions/

# ~ Antidote ---------------------------------------------------------------- ~ #

antidote_dir=${ZDOTDIR:-~/.config/zsh}/.antidote/
plugins=${ZDOTDIR:-~/.config/zsh}/.zsh_plugins.txt
static=${ZDOTDIR:-~/.config/zsh}/.zsh_plugins.zsh

[[ -e $antidote_dir ]] || git clone --depth=1 https://github.com/mattmc3/antidote.git $antidote_dir
fpath+=$antidote_dir
[[ -f $plugins ]] || touch $plugins

autoload -Uz $fpath[-1]/antidote

if [[ ! $static -nt $plugins ]] ; then
    ( antidote bundle <$plugins >$static )
fi

source $static
unset antidote_dir plugins static

# ~ Options ----------------------------------------------------------------- ~ #

bindkey -v

unsetopt SHARE_HISTORY
setopt GLOB_DOTS
setopt COMBINING_CHARS

autoload -Uz promptinit && promptinit
# autoload -Uz compinit && compinit # Done by belak/zsh-utils/completions

# ~ Source ------------------------------------------------------------------ ~ #

source $ZDOTDIR/function.zsh
source $ZDOTDIR/alias.zsh

# ~ Plugin ------------------------------------------------------------------ ~ #

zstyle ':completion:*:git-checkout:*' sort false
zstyle ':completion:*:descriptions' format '[%d]'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'exa -1 --color=always $realpath'
zstyle ':fzf-tab:*' switch-group ',' '.'
zstyle ':fzf-tab:*' fzf-command ftb-tmux-popup
zstyle ':zsh-utils:plugins:history' use-xdg-basedirs

zle -N zle-line-init
eval "$(starship init zsh)"
