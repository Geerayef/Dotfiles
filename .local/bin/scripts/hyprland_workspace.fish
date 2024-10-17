#!/usr/bin/env fish

function main -d "Hyprland utility script: Show current workspace."
    set -f active ●
    set -f inactive ○
    set -f wss 5
    set -f cur (hyprctl activeworkspace | rg --only-matching '\([0-9]?\)' | sed 's/\((\)\([0-9]\)\()\)/\2/')
    set -f msg ""
    for i in (seq 5)
        if test $i -eq $cur
            set msg "$msg $active"
        else
            set msg "$msg $inactive"
        end
    end
    notify-send -u low "$msg"
end

main $argv
