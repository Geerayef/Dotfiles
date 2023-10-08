if status is-interactive
  function fish_user_key_bindings
    fish_vi_key_bindings
  end

  set fzf_fd_opts . --hidden
  set fzf_preview_file_cmd bat --color=always --style "numbers,changes,grid,header" --theme Dracula
  set fzf_preview_dir_cmd exa -laG --color=always --group-directories-first

end

# ~ -------------------------------------------------------------------------------- ~ #

# ~  Prompt

function fish_greeting; end
starship init fish | source
