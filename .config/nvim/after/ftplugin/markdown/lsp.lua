local lsp = require("util.lsp")
local markdown_oxide = {
  on_attach = require("core.func").LspAttach,
  filetypes = { "markdown" },
  root_patterns = { "*.md" },
  cmd = { "markdown-oxide" },
}
lsp.start(markdown_oxide)
