local rust_analyzer = {
  on_attach = require("core.func").LSPAttach,
  filetypes = { "rust" },
  root_patterns = {
    "*.rs",
    "mod.rs",
    "Cargo.toml",
    "Cargo.lock",
    "rust-project.json",
    "package.json",
  },
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

require("util.lsp").start(rust_analyzer)
