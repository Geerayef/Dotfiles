local tsserver = {
  on_attach = require("core.func").LSPAttach,
  init_options = {
    preferences = { disableSuggestions = true },
    hostInfo = "neovim",
  },
  filetypes = { "javascript", "typescript" },
  root_patterns = { "package.json", "jsconfig.json", "tsconfig.json" },
  cmd = { "typescript-language-server", "--stdio" },
}

local biome = {
  on_attach = require("core.func").LSPAttach,
  filetypes = {
    "javascript",
    "typescript",
    "svelte",
    "json",
    "jsonc",
    "css",
  },
  root_patterns = { "package.json", "biome.json", "biome.jsonc" },
  cmd = { "biome", "lsp-proxy" },
}

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() require("util.lsp").start(tsserver) end
  )
end)
vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() require("util.lsp").start(biome) end
  )
end)
