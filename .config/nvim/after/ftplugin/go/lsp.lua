local gopls = {
  name = "Go PLS",
  cmd = { "gopls", "serve" },
  on_attach = LSP.attach,
  filetypes = { "go", "gomod", "gowork", "gotmpl" },
  root_markers = { "*.go", "main.go", "go.mod", "go.work", "go.sum" },
}

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() LSP.start(gopls) end
  )
end)
