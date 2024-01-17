#!/usr/bin/env bash

swayidle -w timeout 600 'swaylock -f' \
  timeout 900 'hyprctl dispatch dpms off' \
  timeout 900 'systemctl suspend' \
  resume 'hyprctl dispatch dpms on' \
  before-sleep 'swaylock -f' &
