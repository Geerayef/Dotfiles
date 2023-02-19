# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# -------------------------------------------------------------------------------- #

# ~  Antidote setup

source ${ZDOTDIR:-~}/.antidote/antidote.zsh

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
alias dotfiles="/usr/bin/git --git-dir=$HOME/Dotfiles/ --work-tree=$HOME"

# -------------------------------------------------------------------------------- #

# ~  Developer Environment

# Rust: Cargo
source "$HOME/.cargo/env"

# Haskell: ghcup
[ -f "/home/tibor/.ghcup/env" ] && source "/home/tibor/.ghcup/env"

# OCaml: opam
[[ ! -r "/home/tibor/.opam/opam-init/init.zsh" ]] || source "/home/tibor/.opam/opam-init/init.zsh"  > /dev/null 2> /dev/null

# -------------------------------------------------------------------------------- #

# ~  General settings

bindkey -e

