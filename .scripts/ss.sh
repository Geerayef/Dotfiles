#!/usr/bin/env bash

screenshot_file_name_template = \"~/Pictures/Screenshots/Screenshot\ from\ $(date '+%Y-%m-%d_%H-%M-%S').png\"

grim -s1 $screenshot_file_name_template
