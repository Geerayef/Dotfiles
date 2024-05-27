#!/usr/bin/env fish

git clone --bare git@github.com:Tibor5/Dotfiles.git "$HOME/Dotfiles"

function dot
    /usr/bin/git --git-dir="$HOME/Dotfiles/" --work-tree="$HOME" $argv
end

if dot checkout
    echo "~~~~~ [INFO] Checked out Dotfiles.\n"
else
    mkdir -p "$HOME/Dotfiles.bkp"
    echo "~~~~~ [INFO] Backing up pre-existing Dotfiles.\n"
    dot checkout 2>&1 | grep -E "\s+\." | awk \{'print $1'\} | xargs -I{} mv {} Dotfiles.bkp/{}
end
dot checkout
dot config status.showUntrackedFiles no
