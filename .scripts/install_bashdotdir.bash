#!/usr/bin/env bash

# ~  Set up the Bash dot directory
# Run as sudo.
# This script defines the location for user's bash config in XDG_CONFIG_HOME.

if [ -d /etc/profile.d ]; then
    if [ -f /etc/profile.d/bashdotdir.sh ]; then
        source /etc/profile.d/bashdotdir.sh
    else
        sudo touch /etc/profile.d/bashdotdir.sh
    fi
fi

bashdotdir="
# ~  Bash dot directory

# Sourced from: /etc/profile
# Located in: /etc/profile.d/
# This script defines the location for user bash configs in XDG_CONFIG_HOME

BASHDOTDIR=\"\${HOME:-/home/\${USER}}/.config/bash/\n
\n
if [ -d \"\$\BASHDOTDIR\" ]; then\n
    source \$BASHDOTDIR/bashrc\n
else\n
    mkdir -p \$BASHDOTDIR\n
    [[ -e \"\$BASHDOTDIR/bashrc\" ]] && source \$BASHDOTDIR/bashrc\n
fi\n
\n
export BASHDOTDIR\n
"

echo $bashdotdir
echo $bashdotdir > /etc/profile.d/bashdotdir.sh
