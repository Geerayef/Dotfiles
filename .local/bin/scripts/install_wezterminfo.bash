#!/usr/bin/env bash

main() {
  local tempfile
  tempfile=$(mktemp)
  curl -o "$tempfile" https://raw.githubusercontent.com/wez/wezterm/main/termwiz/data/wezterm.terminfo
  tic -x -o ~/.terminfo "$tempfile"
  rm "$tempfile"
  printf "~~~~~ [DONE] WezTerm terminfo installed to ~/.terminfo.\n"
}

main
