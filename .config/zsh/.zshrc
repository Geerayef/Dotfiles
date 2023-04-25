# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# -------------------------------------------------------------------------------- #

# ~  Antidote setup

[[ -e $ZDOTDIR/.antidote ]] && source $ZDOTDIR/.antidote/antidote.zsh

# Load plugins
antidote_dir=${ZDOTDIR:-~/.config/zsh}/.antidote
plugins_txt=${ZDOTDIR:-~/.config/zsh}/.zsh_plugins.txt
static_file=${ZDOTDIR:-~/.config/zsh}/.zsh_plugins.zsh

autoload -Uz $antidote_dir/functions/antidote

# Clone antidote if necessary and generate a static plugin file.
[[ -e $antidote_dir ]] || 
  git clone --depth=1 https://github.com/mattmc3/antidote.git $antidote_dir
(
  [[ -e $plugins_txt ]] || touch $plugins_txt
  antidote bundle <$plugins_txt >$static_file
)

source $static_file

unset antidote_dir plugins_txt static_file

autoload -Uz promptinit && promptinit
autoload -Uz compinit && compinit

# -------------------------------------------------------------------------------- #

# ~  Plugin settings

MAGIC_ENTER_GIT_COMMAND='git status .'
MAGIC_ENTER_OTHER_COMMAND='ls -a .'

# fzf-tab
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

# Key bindings
bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down
# bindkey ^R fzf-history-widget

# Prompt: Powerlevel10K
# To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
[[ ! -f ${ZDOTDIR:-~/.config/zsh}/.p10k.zsh ]] || source ${ZDOTDIR:-~/.config/zsh}/.p10k.zsh

# -------------------------------------------------------------------------------- #

# ~  Aliases: source

source $ZDOTDIR/aliases.sh

# -------------------------------------------------------------------------------- #

# ~  Functions

fzf-history-widget() {
      local selected num
      setopt localoptions noglobsubst noposixbuiltins pipefail no_aliases 2> /dev/null
      selected=( $(fc -rl 1 |
      FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} $FZF_DEFAULT_OPTS -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort $FZF_CTRL_R_OPTS --query=${(qqq)LBUFFER} +m" $(__fzfcmd)) )
      local ret=$?
      if [ -n "$selected" ]; then
         num=$selected[1]
         if [ -n "$num" ]; then
            zle vi-fetch-history -n $num
         fi
      fi
      zle reset-prompt
      return $ret
   }
   zle     -N   fzf-history-widget
   bindkey '^R' fzf-history-widget

ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1   ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *.deb)       ar x $1      ;;
      *.tar.xz)    tar xf $1    ;;
      *.tar.zst)   tar xf $1    ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

# -------------------------------------------------------------------------------- #

# ~  General settings

bindkey -e

setopt GLOB_DOTS
unsetopt SHARE_HISTORY

# -------------------------------------------------------------------------------- #

