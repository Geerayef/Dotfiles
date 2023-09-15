#!/usr/bin/env bash

DIR="$HOME/.config/polybar"

killall -q polybar

# Wait for all processes to shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# ~  Launch
polybar -c "$DIR/config.ini" &
# systemctl --user start polybar
