function __complete_syncthing
    set -lx COMP_LINE (commandline -cp)
    test -z (commandline -ct)
    and set COMP_LINE "$COMP_LINE "
    /usr/bin/syncthing
end
complete -f -c syncthing -a "(__complete_syncthing)"
