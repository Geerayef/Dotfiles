#!/usr/bin/env bash

# ~ Create a script that exports BASHDOTDIR=$XDG_CONFIG_HOME/bash and sources it on Bash login.
# ~ Run as sudo.

# @param user string # Path to a user's home directory
main() {
  local user=$1
  if [[ -z $user ]]; then
    printf "~~~~~ [ERROR] User name empty: %s.\n" "$user"
    printf "~~~~~ [     ] Please pass the user name for which to install BASHDOTDIR:\n"
    printf '~~~~~ [     ] e.g. install_bashdotdir $HOME.\n'
    exit
  fi
  printf "~~~~~ Install BASHDOTDIR for user: %s.\n" "$user"
  printf "~~~~~ Create if missing: '/etc/profile.d/', '/etc/profile.d/bashdotdir.sh'.\n"
  [[ ! -d /etc/profile.d ]] && mkdir /etc/profile.d
  [[ ! -f /etc/profile.d/bashdotdir.sh ]] && touch /etc/profile.d/bashdotdir.sh
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
