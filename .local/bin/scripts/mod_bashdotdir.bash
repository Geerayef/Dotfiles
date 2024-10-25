#!/usr/bin/env bash

# ~ Create a script that exports BASHDOTDIR=$XDG_CONFIG_HOME/bash and sources it on Bash login.
# ~ Run as sudo.

# shellcheck shell=bash

THIS
THIS="@$(basename "$0")"

main() {
  if [[ -d $BASHDOTDIR ]]; then
    # shellcheck disable=1091
    . "$BASHDOTDIR/functions/notify.bash"
  else
    # shellcheck disable=1090
    . "${XDG_CONFIG_HOME:-$HOME/.config}bash/functions/notify.bash"
  fi
  local user="$HOME"
  if [[ -z $user ]]; then
    notify "ERROR" "${THIS}: Invalid user name (${user})."
    exit 1
  fi
  notify "INFO" "Installing BASHDOTDIR for user: ${user}."
  notify "INFO" "Create if missing: '/etc/profile.d/'."
  [[ ! -d /etc/profile.d ]] && sudo mkdir /etc/profile.d
  notify "INFO" "Create if missing: '/etc/profile.d/bashdotdir.sh'."
  [[ ! -f /etc/profile.d/bashdotdir.sh ]] && sudo touch /etc/profile.d/bashdotdir.sh
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
