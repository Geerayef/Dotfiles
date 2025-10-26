local biome = {
  name = "Biome LS",
  on_attach = LSP.attach,
  filetypes = {
    "javascript",
    "typescript",
    "svelte",
    "json",
    "jsonc",
    "css",
  },
  root_markers = {
    "package.json",
    "biome.json",
    "biome.jsonc",
    "*.js",
    ".ts",
    "*.json",
    "*.jsonc",
    "*.css",
  },
  cmd = { "biome", "lsp-proxy" },
}

local tslsp = {
  name = "TypeScript LS",
  on_attach = LSP.attach,
  filetypes = { "javascript", "typescript" },
  root_markers = { "package.json", "*.js", "*.ts" },
  cmd = { "typescript-language-server", "--stdio" },
}

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() LSP.start(biome) end
  )
end)

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() LSP.start(tslsp) end
  )
end)
