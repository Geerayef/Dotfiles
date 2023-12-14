#!/usr/bin/env fish

if test -n "$TMUX"
    tmux split-window -h nvim $argv
else if test -e "$(command -v wezterm)" -a "$( /usr/bin/ps -Af | rg 'wezterm-gui' | count )" -gt 1
    set -l wezterm_panes $( wezterm cli list | awk -F ' ' '{print $3}' | rg '[0-9].*' | count )
    if test "$wezterm_panes" -gt 0
        wezterm cli split-pane --horizontal nvim $argv
    else
        wezterm start nvim $argv
    end
else
    if test -e "$(command -v alacritty)"
        alacritty --command nvim $argv
    end
end
