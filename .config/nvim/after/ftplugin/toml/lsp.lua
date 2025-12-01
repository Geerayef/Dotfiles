---@type vim.lsp.Config
local taplo = {
  name = "Taplo LS",
  cmd = { "taplo", "lsp", "stdio" },
  on_attach = LSP.attach,
  filetypes = { "toml" },
  root_markers = { "keymap.toml", "yazi.toml", "*.toml" },
}

vim.schedule(function()
  vim.api.nvim_win_call(vim.api.nvim_get_current_win(), function() LSP.start(taplo) end)
end)
