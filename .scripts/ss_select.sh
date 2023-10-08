#!/usr/bin/env bash

screenshot_file_name_template=~/Pictures/Screenshots/Screenshot\ from\ $(date '+%Y-%m-%d %H-%M-%S').png

# "$HOME/Pictures/Screenshots/Screenshot\ from\ $(date +%Y-%m-%d %H-%M-%S).png"

slurp | grim -g- -s1 -o "test.png"
