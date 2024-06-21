local markdown_oxide = {
  on_attach = require("core.func").LspAttach,
  filetypes = { "markdown" },
  root_patterns = { "*.md" },
  cmd = { "markdown-oxide" },
}

require("util.lsp").start(markdown_oxide)
