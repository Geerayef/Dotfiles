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
  root_patterns = {
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

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() require("util.lsp").start(biome) end
  )
end)
