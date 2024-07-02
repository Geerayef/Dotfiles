local taplo = {
  on_attach = require("core.func").LspAttach,
  filetypes = { "toml" },
  root_patterns = { "keymap.toml", "yazi.toml", "*.toml" },
  cmd = { "taplo", "lsp" },
}

require("util.lsp").start(taplo)
