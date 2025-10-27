local fishlsp = {
  on_attach = LSP.attach,
  cmd = { "fish-lsp", "start" },
  filetypes = { "fish" },
  root_markers = { "*.fish" },
}

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() LSP.start(fishlsp) end
  )
end)
