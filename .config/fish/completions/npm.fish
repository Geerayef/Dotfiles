function __fish_npm_complete
    set -l words (commandline -opc)
    set -l cword (count $words)
    set -l line (commandline -cp)

    set -lx COMP_LINE $line
    set -lx COMP_POINT (string length $line)
    set -lx COMP_CWORD $cword

    npm completion -- $words 2>/dev/null
end

complete -c npm -f -a "(__fish_npm_complete)"
