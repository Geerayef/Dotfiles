# ~  nnn

set -g BLK "0B"
set -g CHR "0B"
set -g DIR "04"
set -g EXE "06"
set -g REG "00"
set -g HARDLINK "06"
set -g SYMLINK "06"
set -g MISSING "00"
set -g ORPHAN "09"
set -g FIFO "06"
set -g SOCK "0B"
set -g OTHER "06"

set -gx NNN_PLUG ""
set -gx NNN_FIFO "/tmp/nnn.fifo"
set -gx NNN_FCOLORS "$BLK$CHR$DIR$EXE$REG$HARDLINK$SYMLINK$MISSING$ORPHAN$FIFO$SOCK$OTHER"

# cd-on-quit only on ^G? Remove "-x"
if test -n "$XDG_CONFIG_HOME"
  set -x NNN_TMPFILE "$XDG_CONFIG_HOME/nnn/.lastd"
else
  set -x NNN_TMPFILE "$HOME/.config/nnn/.lastd"
end

# ~ -------------------------------------------------------------------------------- ~ #

# ~  Functions


# CD on quit

function n --wraps nnn --description 'CD to current directory on exit'
  # Block nesting of nnn in subshells
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
  command nnn -edH $argv

  if test -e $NNN_TMPFILE
    source $NNN_TMPFILE
    rm $NNN_TMPFILE
  end
end
