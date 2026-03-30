---@type vim.lsp.Config
return {
  name = "Markdown Oxide LS",
  cmd = { "markdown-oxide" },
  filetypes = { "markdown" },
  root_markers = { "*.md", ".obsidian/", ".moxide.toml" },
}
