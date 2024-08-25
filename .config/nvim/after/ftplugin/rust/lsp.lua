local rust_analyzer = {
  on_attach = require("core.func").LspAttach,
  filetypes = { "rust" },
  root_patterns = { "*.rs" },
  settings = {
    ["rust-analyzer"] = {
      imports = { prefix = "self", granularity = { group = "module" } },
      cargo = { buildScripts = { enable = true } },
      procMacro = { enable = true },
      checkOnSave = { command = "clippy" },
    },
  },
  cmd = { "rust-analyzer" },
}

LSP.start(rust_analyzer)
