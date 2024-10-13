#!/usr/bin/env fish

function util_get_components -d "Utility: Get available components."
    set -l dir_scripts (status dirname)
    set -l comps (fd "component_" -t f $dir_scripts)
    for i in (seq (count $comps))
        set -l script_basename (basename $comps[$i])
        set comps[$i] $script_basename
    end
    echo $comps
end

function util_usage -d "Utility: Display help." -a available_components
    printf "\nUsage: install.fish [OPTIONS] -- [COMPONENT]\n\n"
    printf "    OPTIONS:\n"
    printf "            -h | --help    -> Display this message.\n\n"
    printf "    Available components:\n"
    for ac in (echo $available_components | string replace -ar 'component_(\w+).[bafi]*sh' '$1' | tr ' ' \n)
        printf "        %s\n" "$ac"
    end
    exit 0
end

function main -d "Install user-space component for which there is a script."
    set -f COMPONENTS (util_get_components)
    argparse -n "install.fish" -X 1 -s h/help -- $argv
    if set -q _flag_h
        util_usage $COMPONENTS
    end
    set -f target $argv[1]
    if test -z "$target"
        notify ERROR "No component is specified."
        exit 1
    end
    set -f script
    for comp in $(echo $COMPONENTS | tr ' ' \n)
        if string match -qe "$target" "$comp"
            set script $comp
            break
        end
    end
    if test -z "$script"
        notify ERROR "No corresponding script for component $target was found."
        exit 1
    end
    notify INFO "Target install script = $script"
    notify INFO "Installing user component: $target..."
    exec $script
    notify DONE "Install component: $target."
end

main $argv
