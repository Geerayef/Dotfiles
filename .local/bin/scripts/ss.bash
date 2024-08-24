#!/usr/bin/env bash

main() {
  local screenshots="$HOME/Pictures/Screenshots"
  local name_template
  name_template="$screenshots/Screenshot $(date '+%d-%m-%Y %T').png"
  case "$1" in
    --selection)
      slurp -d -c "#F9E2AF" -F "Iosevka Nerd Font Mono 20" -w 2 | grim -g- -s1 -t png "$name_template"
      ;;
    *)
      grim -s1 -t png "$name_template"
      ;;
  esac
}

main "$@"
