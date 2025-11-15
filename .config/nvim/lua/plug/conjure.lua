return {
  "Olical/conjure",
  ft = { "scheme", "commonlisp", "lua", "fennel", "python" },
  init = function()
    vim.g["conjure#mapping#enable_defaults"] = false
    vim.g["conjure#extract#tree_sitter#enabled"] = true
    -- Key
    vim.g["conjure#mapping#log_close_visible"] = "Clq"
    vim.g["conjure#mapping#log_buf"] = "Clb"
    vim.g["conjure#mapping#log_split"] = "Cls"
    vim.g["conjure#mapping#log_vsplit"] = "Clv"
    vim.g["conjure#mapping#log_tab"] = "Clt"
    vim.g["conjure#mapping#log_jump_to_latest"] = "Cll"
    vim.g["conjure#mapping#log_reset_soft"] = "Clr"
    vim.g["conjure#mapping#log_reset_hard"] = "ClR"
    vim.g["conjure#mapping#eval_current_form"] = "Cee"
    vim.g["conjure#mapping#eval_comment_current_form"] = "Cec"
    vim.g["conjure#mapping#eval_root_form"] = "Cer"
    vim.g["conjure#mapping#eval_comment_root_form"] = "Cerc"
    vim.g["conjure#mapping#eval_word"] = "Cew"
    vim.g["conjure#mapping#eval_comment_word"] = "Cewc"
    vim.g["conjure#mapping#eval_previous"] = "Cep"
    vim.g["conjure#mapping#eval_replace_form"] = "Ce!"
    vim.g["conjure#mapping#eval_marked_form"] = "Cem"
    vim.g["conjure#mapping#eval_file"] = "Cef"
    vim.g["conjure#mapping#eval_buf"] = "Ceb"
    vim.g["conjure#mapping#eval_visual"] = "CE"
    vim.g["conjure#mapping#eval_motion"] = "CE"
    vim.g["conjure#mapping#def_word"] = { "gd" }
    -- Scheme
    vim.g["conjure#filetype_suffixes#scheme"] = { "ss", "scm" }
    vim.g["conjure#client#scheme#stdio#command"] = "petite"
    vim.g["conjure#client#scheme#stdio#prompt_pattern"] = "> $?"
    vim.g["conjure#client#scheme#stdio#value_prefix_pattern"] = false
  end,
}
