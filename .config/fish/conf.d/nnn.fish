# ~  nnn

set -l BLK E5
set -l CHR E5
set -l DIR 98
set -l EXE 96
set -l REG 6F
set -l HRDL E1
set -l SYML E1
set -l MISS 18
set -l ORPH D3
set -l FIFO 9F
set -l SOCK E5
set -l OTHR D3

set -gx NNN_FIFO "/tmp/nnn.fifo"
set -gx NNN_TERMINAL "$TERM"
set -gx NNN_PAGER bat --style="plain"
set -gx NNN_PLUG "p:-preview.bash"
set -gx NNN_OPTS deEH
set -gx NNN_FCOLORS "$BLK$CHR$DIR$EXE$REG$HRDL$SYML$MISS$ORPH$FIFO$SOCK$OTHR"

if test -n "$XDG_CONFIG_HOME"
    set -x NNN_TMPFILE "$XDG_CONFIG_HOME/nnn/.lastd"
else
    set -x NNN_TMPFILE "$HOME/.config/nnn/.lastd"
end

# ~ -------------------------------------------------------------------------------- ~ #

# ~  Functions

# CD on quit

function n --wraps nnn --description "CD to current directory on exit"
    # Don't nest nnn in subshells
    if test -n "$NNNLVL" -a "$NNNLVL" -ge 1
        echo "nnn is already running"
        return
    end

    # Unmask ^Q (, ^V etc.) (if required, see `stty -a`) to Quit nnn
    stty start undef
    stty stop undef
    # stty lwrap undef
    # stty lnext undef

    command nnn $argv

    if test -e $NNN_TMPFILE
        source $NNN_TMPFILE
        rm $NNN_TMPFILE
    end
end
