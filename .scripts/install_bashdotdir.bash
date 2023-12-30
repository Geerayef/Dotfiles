#!/usr/bin/env bash

# ~  Create a script that exports BASHDOTDIR=$XDG_CONFIG_HOME/bash and sources it on Bash login.
# Run as sudo.

[[ ! -d /etc/profile.d ]] && mkdir /etc/profile.d
[[ ! -f /etc/profile.d/bashdotdir.sh ]] && touch /etc/profile.d/bashdotdir.sh

bashdotdir=$(cat << EOF
# shellcheck shell=bash

# /etc/profile.d/bashdotdir.sh
# Set Bash config location and source it.

[[ -d /home/tibor/.config/bash ]] && export BASHDOTDIR="/home/tibor/.config/bash"
[[ -r $BASHDOTDIR/bashrc ]] && . "$BASHDOTDIR/bashrc"
EOF
)

echo "$bashdotdir" > /etc/profile.d/bashdotdir.sh
