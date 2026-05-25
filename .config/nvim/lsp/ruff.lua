---@type vim.lsp.Config
return {
  name = "Ruff LS",
  cmd = { "ruff", "server" },
  filetypes = { "python" },
  init_options = { settings = { lint = { preview = true } } },
}
