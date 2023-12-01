if status is-interactive
  function fish_user_key_bindings
    fish_vi_key_bindings
  end

  set -gx FZF_DEFAULT_OPTS '--cycle --layout=reverse --border --height=90% --preview-window=wrap --marker="*"'
  set -x fzf_fd_opts . --hidden
  set -x fzf_preview_file_cmd bat
  set -x fzf_preview_dir_cmd eza -aT -L 1 --color=always --icons=always --group-directories-first

end

# ~ -------------------------------------------------------------------------------- ~ #

# ~  Source

# pyenv init - | source

# ~ -------------------------------------------------------------------------------- ~ #

# ~  Prompt

function fish_greeting; end
starship init fish | source

