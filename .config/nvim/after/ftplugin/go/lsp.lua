local gopls = {
  on_attach = require("core.func").LSPAttach,
  filetypes = { "go", "gomod", "gowork", "gotmpl" },
  root_patterns = { "*.go", "main.go", "go.mod", "go.work", "go.sum" },
  cmd = { "gopls" },
}

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() require("util.lsp").start(gopls) end
  )
end)
