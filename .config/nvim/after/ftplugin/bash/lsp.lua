---@type vim.lsp.Config
local bashls = {
  name = "Bash LS",
  cmd = { "bash-language-server", "start" },
  on_attach = LSP.attach,
  filetypes = { "bash", "sh" },
  root_markers = { ".shellcheckrc" },
  settings = { bashIde = { globPattern = "*@(.bash|.inc|.sh|.command)" } },
}

vim.lsp.config("bashls", bashls)

vim.schedule(function()
  vim.api.nvim_win_call(vim.api.nvim_get_current_win(), function() LSP.start(bashls) end)
end)
