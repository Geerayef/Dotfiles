local gopls = {
  on_attach = require("core.func").LspAttach,
  filetypes = { "go", "gomod", "gowork", "gotmpl" },
  root_patterns = { "*.go", "main.go", "go.mod", "go.work", "go.sum" },
  cmd = { "gopls" },
}

require("util.lsp").start(gopls)
