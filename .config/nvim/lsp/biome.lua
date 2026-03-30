---@type vim.lsp.Config
return {
  name = "Biome LS",
  cmd = { "biome", "lsp-proxy" },
  workspace_required = false,
  filetypes = {
    "astro",
    "css",
    "graphql",
    "html",
    "javascript",
    "javascriptreact",
    "json",
    "jsonc",
    "svelte",
    "typescript",
    "typescriptreact",
    "vue",
  },
  root_markers = { "*.js", ".ts", "*.json", "*.jsonc", "*.css", "*.html" },
}
