# shellcheck shell=bash

# /etc/profile.d/bashdotdir.sh
# Set Bash config location and source it.

[[ -d $HOME/.config/bash ]] && export BASHDOTDIR="$HOME/.config/bash"
[[ -r $BASHDOTDIR/bashrc ]] && . "$BASHDOTDIR/bashrc"
