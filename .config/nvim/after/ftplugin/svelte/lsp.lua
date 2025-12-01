---@type vim.lsp.Config
local svelte = {
  name = "Svelte LS",
  cmd = { "svelteserver", "--stdio" },
  on_attach = LSP.attach,
  filetypes = { "svelte" },
  root_markers = { "biome.json", "biome.jsonc", "svelte.config.js" },
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
  vim.api.nvim_win_call(vim.api.nvim_get_current_win(), function() LSP.start(svelte) end)
end)
vim.schedule(function()
  vim.api.nvim_win_call(vim.api.nvim_get_current_win(), function() LSP.start(biome) end)
end)
