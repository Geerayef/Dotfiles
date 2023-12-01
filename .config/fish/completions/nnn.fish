# ~  nnn - Fish completions

if test -n "$XDG_CONFIG_HOME"
    set sessions_dir $XDG_CONFIG_HOME/.config/nnn/sessions
else
    set sessions_dir $HOME/.config/nnn/sessions
end

complete --command nnn -s a    -d 'auto-create NNN_FIFO'
complete --command nnn -s A    -d 'disable dir auto-enter'
complete --command nnn -s b -r -d 'bookmark key to open' -x -a '(echo $NNN_BMS | awk -F: -v RS=\; \'{print $1"\t"$2}\')'
complete --command nnn -s B    -d 'use bsdtar for archives'
complete --command nnn -s c    -d 'cli-only opener'
complete --command nnn -s C    -d 'color by context'
complete --command nnn -s d    -d 'start in detail mode'
complete --command nnn -s D    -d 'dirs in context color'
complete --command nnn -s e    -d 'open text files in $VISUAL/$EDITOR/vi'
complete --command nnn -s E    -d 'use EDITOR for undetached edits'
complete --command nnn -s f    -d 'use readline history file'
complete --command nnn -s g    -d 'regex filters'
complete --command nnn -s H    -d 'show hidden files'
complete --command nnn -s i    -d 'show current file info'
complete --command nnn -s J    -d 'no auto-advance on selection'
complete --command nnn -s K    -d 'detect key collision and exit'
complete --command nnn -s l -r -d 'lines to move per scroll'
complete --command nnn -s n    -d 'start in type-to-nav mode'
complete --command nnn -s o    -d 'open files only on Enter'
complete --command nnn -s p -r -d 'copy selection to file' -a '-\tstdout'
complete --command nnn -s P -r -d 'plugin key to run' -x -a '(echo $NNN_PLUG | awk -F: -v RS=\; \'{print $1"\t"$2}\')'
complete --command nnn -s Q    -d 'disable quit confirmation'
complete --command nnn -s r    -d 'show cp, mv progress (Linux-only)'
complete --command nnn -s R    -d 'disable rollover at edges'
complete --command nnn -s s -r -d 'load session by name' -x -a '@\t"last session" (ls $sessions_dir)'
complete --command nnn -s S    -d 'persistent session'
complete --command nnn -s t -r -d 'timeout in seconds to lock'
complete --command nnn -s T -r -d 'a d e r s t v'
complete --command nnn -s u    -d 'use selection (no prompt)'
complete --command nnn -s U    -d 'show user and group'
complete --command nnn -s V    -d 'show program version and exit'
complete --command nnn -s x    -d 'notis, sel to system clipboard, xterm title'
complete --command nnn -s h    -d 'show program help'
