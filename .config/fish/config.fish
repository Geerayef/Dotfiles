set -U fish_greeting

if status is-interactive
    test -r "$HOME/.opam/opam-init/init.fish" && source "$HOME/.opam/opam-init/init.fish" >/dev/null 2>/dev/null
    fnm env --use-on-cd --corepack-enabled --version-file-strategy=recursive --shell fish | source
    starship init fish | source
end
