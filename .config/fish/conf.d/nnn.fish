# ~  nnn

set -gx NNN_PLUG ''
set -gx NNN_COLORS ''
set -gx NNN_PAGER 'nvim -R'
set -gx NNN_FIFO '/tmp/nnn.fifo'

# To cd on quit only on ^G, remove the "-x" from both lines below, without changing the paths.
if test -n "$XDG_CONFIG_HOME"
  set -x NNN_TMPFILE "$XDG_CONFIG_HOME/nnn/.lastd"
else
  set -x NNN_TMPFILE "$HOME/.config/nnn/.lastd"
end

# ~ -------------------------------------------------------------------------------- ~ #

# ~ Functions


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
