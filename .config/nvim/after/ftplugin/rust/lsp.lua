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
      checkOnSave = false,
    },
  },
  cmd = { "rust-analyzer" },
}

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() require("util.lsp").start(rust_analyzer) end
  )
end)
