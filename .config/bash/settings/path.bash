# ~  Path

[[ -d "$HOME/.bin" ]] && export PATH="$HOME/.bin:$PATH"

[[ -d "$HOME/.local/bin" ]] && export PATH="$HOME/.local/bin:$PATH"

[[ -d "$HOME/.scripts/" ]] && export PATH="$HOME/.scripts/:$PATH"

[[ -d "$HOME/Software/Neovim/bin" ]] && export PATH="$HOME/Software/Neovim/bin:$PATH"

[[ -d "$HOME/linuxbrew/" ]] && export PATH="$HOME/linuxbrew/.linuxbrew/bin/" && eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"

[[ -d $ANDROID_HOME ]] && export PATH="$ANDROID_HOME/tools:$ANDROID_HOME/tools/bin:$ANDROID_HOME/platform-tools:$PATH"

[[ -d $JAVA_HOME ]] && export PATH="$JAVA_HOME:$PATH"
