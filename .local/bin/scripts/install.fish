#!/usr/bin/env fish

function util_get_components -d "Utility: Get the list of available components."
    set -l dir_scripts (status dirname)
    set -l comps (fd "install_" -t f $dir_scripts)
    for i in (seq (count $comps))
        set -l script_basename (basename $comps[$i])
        set comps[$i] $script_basename
    end
    echo $comps
end

function util_usage -d "Display help (usage)." -a available_components
    printf "\n    Usage: install.fish [OPTION] -- [COMPONENT]\n\n"
    printf "    OPTIONS:\n"
    printf "            -h | --help    -> Display this message.\n\n"
    printf "    Available components:\n"
    for ac in (echo $available_components | tr ' ' \n)
        printf "        %s\n" "$ac"
    end
    exit 0
end

function util_handle_args -d "Handle CLI arguments." -a clargs available_components
    set -l targets
    # TODO: Find a better way to split args than this (echo $clargs | tr ' ' \n)
    for arg in (echo $clargs | tr ' ' \n)
        set -a targets $arg
        # string match -q -- '-*' "$arg"
        # if test $status -eq 0
        #     echo HELPIN
        #     util_usage $available_components
        #     exit 0
        # end
        # switch "$arg"
        #     case -h --help
        #         util_usage $available_components
        #     case '*'
        #         set -a targets "$arg"
        # end
    end
    echo $targets
end

function main -d "Install user-space component for which there is a script."
    set -l COMPONENTS (util_get_components)
    echo "@main: CLI arguments: $argv"
    echo "@main: Available components: $COMPONENTS"
    set -l install_candidates (util_handle_args $argv $COMPONENTS)
    echo "@main: Install targets: $install_candidates"
    # for i in (echo $install_candidates | tr ' ' \n)
    #     notify INFO "@main: Candidate = $i"
    # end
    exit 0
    set -l component_script
    for script in $COMPONENTS
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
