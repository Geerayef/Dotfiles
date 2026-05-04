---@type vim.lsp.Config
return {
  name = "Rust LS",
  cmd = { "rust-analyzer" },
  filetypes = { "rust" },
  root_markers = GRIM.static.root_markers.rust,
  settings = {
    ["rust-analyzer"] = {
      imports = { prefix = "self", granularity = { group = "module" } },
      cargo = { buildScripts = { enable = true } },
      checkOnSave = false,
      lens = {
        debug = { enable = true },
        enable = true,
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
