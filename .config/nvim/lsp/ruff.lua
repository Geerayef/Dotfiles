---@type vim.lsp.Config
return {
  name = "Ruff LS",
  cmd = { "ruff", "server" },
  filetypes = { "python" },
  root_markers = { "*.py" },
  init_options = { settings = { lint = { preview = true } } },
}
