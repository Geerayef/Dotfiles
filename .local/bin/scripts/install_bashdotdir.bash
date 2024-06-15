#!/usr/bin/env bash

# ~ Create a script that exports BASHDOTDIR=$XDG_CONFIG_HOME/bash and sources it on Bash login.
# ~ Run as sudo.

# @param user string # Path to a user's home directory
main() {
  printf "~~~~~ Install BASHDOTDIR.\n"
  printf "~~~~~ Create if missing: '/etc/profile.d/', '/etc/profile.d/bashdotdir.sh'.\n"
  [[ ! -d /etc/profile.d ]] && mkdir /etc/profile.d
  [[ ! -f /etc/profile.d/bashdotdir.sh ]] && touch /etc/profile.d/bashdotdir.sh
  local user=$1
  local bashdotdir
  bashdotdir=$(
    cat <<EOF
# shellcheck shell=bash

# /etc/profile.d/bashdotdir.sh
# Set Bash config location and source it.

[[ -d $user/.config/bash ]] && export BASHDOTDIR="$user/.config/bash"
[[ -r \$BASHDOTDIR/bashrc ]] && . "\$BASHDOTDIR/bashrc"
EOF
  )
  echo "$bashdotdir" >/etc/profile.d/bashdotdir.sh
}

main "$@"
