# ~  Path

[[ -d "$HOME/.local/bin" ]] && export PATH="$HOME/.local/bin:$PATH"

[[ -d "$HOME/.local/bin/scripts" ]] && export PATH="$HOME/.local/bin/scripts:$PATH"

[[ -d "$HOME/.local/share/nvim/mason/bin" ]] && export PATH="$HOME/.local/share/nvim/mason/bin:$PATH"

[[ -d "$HOME/.cargo" ]] && export PATH="$HOME/.cargo/bin:$PATH"

[[ -d "$HOME/.opam/default/bin" ]] && export PATH="$HOME/.opam/default/bin:$PATH"

[[ -d "$HOME/.local/texlive" ]] && export PATH="$HOME/.local/texlive/2024/bin/x86_64-linux:$PATH"

[[ -d "$HOME/.pyenv" ]] && export PATH="$HOME/.pyenv/bin:$PATH"

[[ -d "$HOME/linuxbrew/" ]] && export PATH="$HOME/linuxbrew/.linuxbrew/bin/" && eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"

[[ -d $ANDROID_HOME ]] && export PATH="$ANDROID_HOME/tools:$ANDROID_HOME/tools/bin:$ANDROID_HOME/platform-tools:$PATH"

[[ -d $JAVA_HOME ]] && export PATH="$JAVA_HOME:$PATH"
