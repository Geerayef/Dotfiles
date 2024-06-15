#!/usr/bin/env bash

main() {
  local screenshots="$HOME/Pictures/Screenshots"
  local name_template
  name_template="$screenshots/Screenshot $(date '+%d-%m-%Y %T').png"
  case "$1" in
    --selection)
      slurp | grim -g- -s1 "$name_template"
      ;;
    *)
      grim -s1 "$name_template"
      ;;
  esac
}

main "$@"
