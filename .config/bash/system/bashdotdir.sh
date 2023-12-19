# shellcheck shell=bash

# /etc/profile.d/bashdotdir.sh
# Set Bash config location and soruce it.

[[ -d /home/tibor/.config/bash ]] && export BASHDOTDIR="/home/tibor/.config/bash"
[[ -r $BASHDOTDIR/rc.bash ]] && . $BASHDOTDIR/rc.bash
