---@type vim.lsp.Config
return {
  name = "Rust LS",
  cmd = { "rust-analyzer" },
  filetypes = { "rust" },
  root_markers = { "*.rs" },
  settings = {
    ["rust-analyzer"] = {
      imports = { prefix = "self", granularity = { group = "module" } },
      cargo = { buildScripts = { enable = true } },
      checkOnSave = false,
    },
  },
}
