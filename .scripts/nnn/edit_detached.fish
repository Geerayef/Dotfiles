#!/usr/bin/env fish

if test -n "$TMUX"
    tmux split-window -h nvim $argv
else
    set -l wezterm_active (wezterm cli list | awk -F ' ' '{print $3}' | count)
    if test $wezterm_active -gt 1
        wezterm cli split-pane --horizontal nvim $argv
    else
        wezterm start nvim $argv
    end
end

# ~  TODO: Alacritty as backup
# if test -e "$(command -v alacritty)"
#     alacritty --command nvim $argv
# end
