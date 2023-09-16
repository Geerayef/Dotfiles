#!/bin/sh

xrandr --output eDP-1 --mode 1920x1080 --fb 5760x3240 --pos 0x0 --scale 1x1 --rotate normal \
 --output DP-2-8  --mode 3840x2160 --pos 1920x0 --scale 1x1 --rotate normal --rate 30
