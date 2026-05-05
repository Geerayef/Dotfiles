---@type vim.lsp.Config
return {
  name = "Fish LSP",
  cmd = { "fish-lsp", "start" },
  filetypes = { "fish" },
  root_markers = GRIM.static.root_markers.fish,
}
