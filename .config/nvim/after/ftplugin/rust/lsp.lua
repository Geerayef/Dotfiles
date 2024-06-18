local lsp = require("util.lsp")
local rust_analyzer = {
  filetypes = { "rust" },
  root_patterns = { "*.rs" },
  settings = {
    ["rust-analyzer"] = {
      imports = { prefix = "self", granularity = { group = "module" } },
      cargo = { buildScripts = { enable = true } },
      procMacro = { enable = true },
    },
  },
  cmd = { "rust-analyzer" },
}

lsp.start(rust_analyzer)
