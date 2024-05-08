#!/usr/bin/env fish

if test -n "$TMUX"
    tmux split-window -h $EDITOR $argv
else if test -n "$CURRENT_TERM"
    switch $CURRENT_TERM
        case wezterm
            if test -e "$(command -v wezterm)" -a "$( /usr/bin/ps -Af | rg 'wezterm-gui' | count )" -gt 1
                set -l wezterm_panes $( wezterm cli list | awk -F ' ' '{print $3}' | rg '[0-9].*' | count )
                if test "$wezterm_panes" -gt 0
                    wezterm cli split-pane --horizontal $EDITOR $argv
                else
                    wezterm start $EDITOR $argv
                end
            end
        case alacritty
            if test -e "$(command -v alacritty)"
                alacritty --command $EDITOR $argv
            end
        case foot
            if test -e "$(command -v foot)"
                foot -e $EDITOR $argv
            end
        case kitty
            if test -e "$(command -v kitty)"
                kitty $EDITOR $argv
            end
        case '*'
            echo "~~~~~ [INFO] Environment variable CURRENT TERM is not set."
    end
else
    $TERM -e $EDITOR $argv
end
