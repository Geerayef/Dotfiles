#!/usr/bin/env fish

function main -d "Install WezTerm terminfo."
    set -f tempfile
    set tempfile (mktemp)
    curl -o "$tempfile" https://raw.githubusercontent.com/wez/wezterm/main/termwiz/data/wezterm.terminfo
    tic -x -o ~/.terminfo "$tempfile"
    rm "$tempfile"
    notify DONE "Install WezTerm terminfo to ~/.terminfo."
end

main
