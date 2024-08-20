#!/usr/bin/env fish

function main -d "Install user-space component for which there is a script."
    printf "~~~~~ %s\n" "Install user-space component for which there is a script."
    set -l component (string lower "$argv[1]")
    set -l dir_scripts (status dirname)
    set -l installables (fd "install_" -t f $dir_scripts)
    for i in (seq (count $installables))
        set -l script $installables[$i]
        set -l script_basename (basename $script)
        set installables[$i] $script_basename
    end
    set -l component_script
    for script in $installables
        if string match -q "*$component*" "$script"
            set component_script $script
            break
        end
    end
    if test (string length "$component_script") -eq 0
        notify ERROR "Selected component ($component) is invalid."
        return 1
    end
    notify INFO "Installing $component."
    exec $component_script
    notify DONE "Install $component."
end

main $argv
