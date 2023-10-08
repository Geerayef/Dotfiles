#!/usr/bin/env bash

if [[ -e ~/.ssh/ ]] ; then
    git clone --bare git@github.com:Tibor5/Dotfiles.git $HOME/Dotfiles
else
    git clone --bare https://www.github.com/Tibor5/Dotfiles.git $HOME/Dotfiles
fi

function dot {
   /usr/bin/git --git-dir=$HOME/Dotfiles/ --work-tree=$HOME $@
}

mkdir -p Dotfiles.bkp 
dot checkout
if [ $? = 0 ]; then
  echo "Checked out config.";
  else
    echo "Backing up pre-existing dot files.";
    dot checkout 2>&1 | grep -E "\s+\." | awk {'print $1'} | xargs -I{} mv {} Dotfiles.bkp/{}
fi;
dot checkout
dot config status.showUntrackedFiles no
