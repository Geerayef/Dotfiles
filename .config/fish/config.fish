if status is-interactive
  function fish_user_key_bindings
    fish_vi_key_bindings
  end

end

# ~ -------------------------------------------------------------------------------- ~ #

# ~  Prompt

function fish_greeting; end
starship init fish | source
