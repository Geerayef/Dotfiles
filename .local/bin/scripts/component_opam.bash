#!/usr/bin/env bash

# shellcheck shell=bash

THIS
THIS="@$(basename "$0")"

check_dependencies() {
  notify "INFO" "${THIS}: Checking dependencies..."
  local dependencies=("gcc" "base-devel" "curl" "bubblewrap" "unzip")
  for dep in "${dependencies[@]}"; do
    if [[ ! "$(command -v "$dep")" ]]; then
      notify "ERROR" "${THIS}: Dependency ${dep} is not installed."
      notify "INFO" "${THIS}: Make sure to install the dependencies:"
      echo "${dependencies[@]}"
      exit 1
    fi
  done
}

main() {
  if [[ -d $BASHDOTDIR ]]; then
    # shellcheck disable=1091
    . "$BASHDOTDIR/functions/notify.bash"
  else
    # shellcheck disable=1090
    . "${XDG_CONFIG_HOME:-$HOME/.config}bash/functions/notify.bash"
  fi
  check_dependencies
  bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
  if [[ "$(command -v opam)" ]]; then
    opam init
  else
    notify "ERROR" "${THIS}: Command 'opam' not found. Restart the shell."
  fi
}

main
