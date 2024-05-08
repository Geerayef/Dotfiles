#!/usr/bin/env bash

SCREENSHOTS_DIR=$HOME/Pictures/Screenshots
screenshot_file_name_template=$SCREENSHOTS_DIR/Screenshot_from_$(date '+%d-%m-%Y %T').png

case "$1" in
  --selection)
    slurp | grim -g- -s1 "$screenshot_file_name_template"
    ;;
  *)
    grim -s1 "$screenshot_file_name_template"
    ;;
esac
