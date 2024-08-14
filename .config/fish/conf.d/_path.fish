if test -d "$HOME/.local/bin"
    fish_add_path -a --path "$HOME/.local/bin"
end

if test -d "$HOME/.local/bin/scripts"
    fish_add_path -a --path "$HOME/.local/bin/scripts"
end

if test -d "$HOME/.local/share/nvim/mason/bin"
    fish_add_path -a --path "$HOME/.local/share/nvim/mason/bin"
end

# ~  Developer Environment

# opam
if test -d "$HOME/.opam"
    set -l switch (opam switch show)
    not contains "$HOME/.opam/$switch/bin" $PATH
    # and fish_add_path -a --path "$HOME/.opam/$switch/bin"
    and source "$HOME/.opam/opam-init/init.fish"
end

# Cargo
if test -d "$HOME/.cargo"
    fish_add_path -a --path "$HOME/.cargo/bin"
end

if test -e "$(command -v go)"
    set -l gobin (go env GOBIN)
    fish_add_path -a --path "$gobin"
    set -l gopath (go env GOPATH)/bin
    fish_add_path -a --path "$gopath"
end

# pnpm
if not string match -q -- $PNPM_HOME $PATH
    fish_add_path -a --path "$PNPM_HOME"
end

# ghcup
if test -d "$HOME/.ghcup"
    fish_add_path -a --path "$HOME/.ghcup/bin"
end

# pyenv
if test -e "$HOME/.pyenv"
    set -gx PYENV_ROOT "$HOME/.pyenv"
    and fish_add_path -a --path "$PYENV_ROOT/bin"
end

# Tex
if test -d /usr/local/texlive
    fish_add_path -p --path /usr/local/texlive/2024/bin/x86_64-linux
end
if test -d "$HOME/.local/texlive/"
    fish_add_path -p --path "$HOME/.local/texlive/2024/bin/x86_64-linux"
end
