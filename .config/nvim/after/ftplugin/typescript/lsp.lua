vim.o.tabstop = 2
vim.o.shiftwidth = 2

---@type vim.lsp.Config
local tsserver = {
  name = "TypeScript LS",
  cmd = { "typescript-language-server", "--stdio" },
  on_attach = LSP.attach,
  init_options = {
    preferences = { disableSuggestions = true },
    hostInfo = "neovim",
  },
  filetypes = { "javascript", "typescript" },
  root_markers = { "package.json", "jsconfig.json", "tsconfig.json" },
}

---@type vim.lsp.Config
local biome = {
  name = "Biome LS",
  cmd = { "biome", "lsp-proxy" },
  on_attach = LSP.attach,
  filetypes = {
    "javascript",
    "typescript",
    "svelte",
    "json",
    "jsonc",
    "css",
  },
  root_markers = { "package.json", "biome.json", "biome.jsonc" },
}

vim.schedule(function()
  vim.api.nvim_win_call(vim.api.nvim_get_current_win(), function() LSP.start(tsserver) end)
end)
vim.schedule(function()
  vim.api.nvim_win_call(vim.api.nvim_get_current_win(), function() LSP.start(biome) end)
end)
