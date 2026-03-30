---@type vim.lsp.Config
return {
  name = "Taplo LS",
  cmd = { "taplo", "lsp", "stdio" },
  filetypes = { "toml" },
  root_markers = { "*.toml" },
}
