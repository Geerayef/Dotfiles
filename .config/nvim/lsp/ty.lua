---@type vim.lsp.Config
return {
  name = "Ty LS",
  cmd = { "ty", "server" },
  filetypes = { "python" },
  root_markers = GRIM.static.root_markers.python,
}
