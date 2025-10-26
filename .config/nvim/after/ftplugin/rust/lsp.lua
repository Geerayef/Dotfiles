local rust_analyzer = {
  name = "Rust Analyzer LS",
  cmd = { "rust-analyzer" },
  on_attach = LSP.attach,
  filetypes = { "rust" },
  root_markers = {
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
      checkOnSave = false,
    },
  },
}

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() LSP.start(rust_analyzer) end
  )
end)
