# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# -------------------------------------------------------------------------------- #

# ~  Antidote setup

[[ -e .config/zsh/.antidote ]] && source ${ZDOTDIR:-~}/.antidote/antidote.zsh

# Load plugins
antidote_dir=${ZDOTDIR:-~}/.antidote
plugins_txt=${ZDOTDIR:-~}/.zsh_plugins.txt
static_file=${ZDOTDIR:-~}/.zsh_plugins.zsh

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

# Prompt: Powerlevel10K
# To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
[[ ! -f ${ZDOTDIR:-~}/.p10k.zsh ]] || 
  source ${ZDOTDIR:-~}/.p10k.zsh

# -------------------------------------------------------------------------------- #

# ~  Aliases

alias nv="nvim"
alias code="/usr/bin/codium"
alias dotfiles="/usr/bin/git --git-dir=$HOME/Dotfiles/ --work-tree=$HOME"

# List
alias l.="ls -A | egrep '^\.'"

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

#give the list of all installed desktops - xsessions desktops
alias xd="ls /usr/share/xsessions"
alias xdw="ls /usr/share/wayland-sessions"

# -------------------------------------------------------------------------------- #

# ~  Developer Environment

# Rust: Cargo
[[ -e "$HOME/.cargo/" ]] && source "$HOME/.cargo/env"

# Haskell: ghcup
[ -f "/home/tibor/.ghcup/env" ] && source "/home/tibor/.ghcup/env"

# OCaml: opam
[[ ! -r "/home/tibor/.opam/opam-init/init.zsh" ]] || source "/home/tibor/.opam/opam-init/init.zsh"  > /dev/null 2> /dev/null

# -------------------------------------------------------------------------------- #

# ~  General settings

bindkey -e

setopt GLOB_DOTS
unsetopt SHARE_HISTORY
