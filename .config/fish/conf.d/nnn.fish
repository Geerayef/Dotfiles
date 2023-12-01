# ~  nnn

set -l BLK "0B"
set -l CHR "0B"
set -l DIR "B4" # 90
set -l EXE "31"
set -l REG "FB"
set -l HRDL "7C"
set -l SYML "D0"
set -l MISS "00"
set -l ORPH "09"
set -l FIFO "06"
set -l SOCK "0B"
set -l OTHR "4B"

set -gx NNN_FIFO "/tmp/nnn.fifo"
set -gx NNN_TERMINAL "$TERM"
set -gx NNN_PAGER "bat"
set -gx NNN_OPTS "deH" 
set -gx NNN_PLUG "p:-preview-tui"
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

    # nnn alias
    command nnn $argv

    if test -e $NNN_TMPFILE
        source $NNN_TMPFILE
        rm $NNN_TMPFILE
    end
end
