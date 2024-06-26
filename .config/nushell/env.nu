$env.ENV_CONVERSIONS = {
    "PATH": {
        from_string: { |s| $s | split row (char esep) | path expand --no-symlink }
        to_string: { |v| $v | path expand --no-symlink | str join (char esep) }
    }
    "Path": {
        from_string: { |s| $s | split row (char esep) | path expand --no-symlink }
        to_string: { |v| $v | path expand --no-symlink | str join (char esep) }
    }
}

$env.NU_LIB_DIRS = [
    ($nu.default-config-dir | path join 'scripts')
    ($nu.data-dir | path join 'completions')
]

$env.NU_PLUGIN_DIRS = [
    ($nu.default-config-dir | path join 'plugins')
]

# ~ Path -------------------------------------------------------------------- ~ #

use std "path add"
$env.PATH = ($env.PATH | split row (char esep))
path add ($env.HOME | path join ".opam" "bin")
path add ($env.HOME | path join ".pyenv" "bin")
path add ($env.HOME | path join ".cargo" "default" "bin")
path add ($env.HOME | path join ".local" "bin")
path add ($env.HOME | path join ".local" "bin" "scripts")
path add ($env.HOME | path join ".local" "share" "nvim" "mason" "bin")
path add ($env.HOME | path join ".local" "texlive" "2024" "bin" "x86_64-linux")
$env.PATH = ($env.PATH | uniq)

# source ($nu.default-config-dir | path join 'custom.nu')

starship init nu | save -f ~/.cache/starship/init.nu
