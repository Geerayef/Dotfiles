local lsp = require("util.lsp")
local markdown_oxide = {
  filetypes = { "markdown" },
  root_patterns = { "*.md" },
  cmd = { "markdown-oxide" },
}
lsp.start(markdown_oxide)
