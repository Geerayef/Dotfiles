#!/usr/bin/zsh
if [[ -e ~/.ssh/ ]] ; then
    git clone --bare git@github.com:Tibor5/Dotfiles.git $HOME/Dotfiles
else
    git clone --bare https://www.github.com/Tibor5/Dotfiles.git $HOME/Dotfiles
fi

function dtfls {
   /usr/bin/git --git-dir=$HOME/Dotfiles/ --work-tree=$HOME $@
}

mkdir -p Dotfiles.bkp 
dtfls checkout
if [ $? = 0 ]; then
  echo "Checked out config.";
  else
    echo "Backing up pre-existing dot files.";
    dtfls checkout 2>&1 | grep -E "\s+\." | awk {'print $1'} | xargs -I{} mv {} Dotfiles.bkp/{}
fi;
dtfls checkout
dtfls config status.showUntrackedFiles no
