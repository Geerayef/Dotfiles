---@type vim.lsp.Config
return {
  name = "Rust LS",
  cmd = { "rust-analyzer" },
  filetypes = { "rust" },
  settings = {
    ["rust-analyzer"] = {
      imports = { prefix = "self", granularity = { group = "module" } },
      cargo = { buildScripts = { enable = true } },
      checkOnSave = false,
      lens = {
        enable = true,
        debug = { enable = true },
        implementations = { enable = true },
        references = {
          adt = { enable = true },
          enumVariant = { enable = true },
          method = { enable = true },
          trait = { enable = true },
        },
        run = { enable = true },
        updateTest = { enable = true },
      },
    },
  },
  capabilities = {
    experimental = {
      commands = {
        commands = {
          "rust-analyzer.showReferences",
          "rust-analyzer.runSingle",
          "rust-analyzer.debugSingle",
        },
      },
      serverStatusNotification = true,
    },
  },
}
