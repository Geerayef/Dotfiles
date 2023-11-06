if status is-interactive
  function fish_user_key_bindings
    fish_vi_key_bindings
  end

  set -gx FZF_DEFAULT_OPTS '--cycle --layout=reverse --border --height=90% --preview-window=wrap --marker="*"'
  set fzf_fd_opts . --hidden
  set fzf_preview_file_cmd bat --color=always --style "numbers,changes,grid,header" --theme Dracula
  set fzf_preview_dir_cmd eza -laG --color=always --group-directories-first

end

# ~ -------------------------------------------------------------------------------- ~ #

# ~  Source

# opam
source /home/tibor/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true

# ~ -------------------------------------------------------------------------------- ~ #

# ~  Prompt

function fish_greeting; end
starship init fish | source

