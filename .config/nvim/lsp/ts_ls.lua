---@type vim.lsp.Config
return {
  name = "TypeScript LS",
  cmd = { "typescript-language-server", "--stdio" },
  init_options = { hostInfo = "neovim" },
  filetypes = { "javascript", "typescript" },
  root_markers = GRIM.static.root_markers.javascript,
}
