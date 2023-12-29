# ~  ZSH

# Environment variables
[[ -n $BASHDOTDIR || -r $HOME/.config/bash/settings/env.bash ]] && source ./.config/bash/settings/env.bash

# Path
[[ -n $BASHDOTDIR || -r $XDG_CONFIG_HOME/bash/settings/path.bash ]] && source $HOME/.config/bash/settings/path.bash
