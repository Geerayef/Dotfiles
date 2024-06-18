local lsp = require("util.lsp")
local svelte = {
  filetypes = { "svelte" },
  root_patterns = { "biome.json", "biome.jsonc", "svelte.config.js" },
  cmd = { "svelteserver", "--stdio" },
}
local biome = {
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
lsp.start(svelte)
lsp.start(biome)
