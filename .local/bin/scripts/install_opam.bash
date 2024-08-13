#!/usr/bin/env bash

check_dependencies() {
  printf "~~~~~ [INFO] Check dependencies.\n"
  local dependencies=("gcc" "base-devel" "curl" "bubblewrap" "unzip")
  for dep in "${dependencies[@]}"; do
    if [[ ! "$(command -v "$dep")" ]]; then
      printf "~~~~~ [INFO] Dependency %s is not installed.\n~~~~~ Make sure to install the necessary dependencies.\n" "$dep"
    fi
  done
}

main() {
  printf "~~~~~ Install OCaml.\n"
  check_dependencies
  bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
  if [[ "$(command -v opam)" ]]; then
    opam init
  else
    printf "~~~~~ [INFO] 'opam' not found. Restart the shell.\n"
  fi
}

main
