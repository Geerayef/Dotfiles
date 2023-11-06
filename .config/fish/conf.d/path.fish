# ~  Add path

if test -d "$HOME/.bin"
  set -x PATH "$HOME/.bin" $PATH
end

if test -d "$HOME/.local/bin"
  set -x PATH "$HOME/.local/bin" $PATH
end

if test -d "$HOME/.local/share/nvim/mason/"
  set -x PATH "$HOME/.local/share/nvim/mason/bin" $PATH
end

if test -d "$HOME/Software/Neovim/bin"
  set -x PATH "$HOME/Software/Neovim/bin" $PATH
end

if test -d "$HOME/.scripts/"
  set -x PATH "$HOME/.scripts" $PATH
end

# ~  Developer Environment

# Rust: Cargo
if test -d "$HOME/.cargo"
  fish_add_path "$HOME/.cargo/bin"
end

# Haskell: ghcup
if test -d "$HOME/.ghcup"
  fish_add_path "$HOME/.ghcup/bin"
end

# OCaml: opam
if test -d "$HOME/.opam"
  and test -r "$HOME/.opam/opam-init/init.fish"
  source "$HOME/.opam/opam-init/init.fish"
end

# Python: pyenv
if test -e "$HOME/.pyenv"
  set -gx PYENV_ROOT "$HOME/.pyenv"
  command -v pyenv >/dev/null
  or set -gx PATH $PATH "$PYENV_ROOT/bin"
  eval "$(pyenv init -)"
end
