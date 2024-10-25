#!/usr/bin/env bash

# shellcheck shell=bash

THIS
THIS="@$(basename "$0")"

function dot() {
  /usr/bin/git --git-dir="$HOME/Dotfiles/" --work-tree="$HOME" "$@"
}

main() {
  if [[ -d $BASHDOTDIR ]]; then
    # shellcheck disable=1091
    . "$BASHDOTDIR/functions/notify.bash"
  else
    # shellcheck disable=1090
    . "${XDG_CONFIG_HOME:-$HOME/.config}bash/functions/notify.bash"
  fi
  notify "INFO" "Cloning Dotfiles to $HOME/Dotfiles..."
  git clone --bare git@github.com:Geerayef/Dotfiles.git "$HOME/Dotfiles"
  {
    if dot checkout; then
      notify "INFO" "Checked out Dotifles."
    else
      mkdir -p "$HOME/Dotfiles.bkp"
      notify "INFO" "Backing up pre-existing Dotfiles."
      dot checkout 2>&1 | grep -E "\s+\." | awk \{'print $1'\} | xargs -I{} mv {} Dotfiles.bkp/{}
    fi
    dot checkout
    dot config status.showUntrackedFiles no
  }
}

main
