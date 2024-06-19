local lsp = require("util.lsp")
local svelte = {
  on_attach = require("core.func").LspAttach,
  filetypes = { "svelte" },
  root_patterns = { "biome.json", "biome.jsonc", "svelte.config.js" },
  cmd = { "svelteserver", "--stdio" },
}
local biome = {
  on_attach = require("core.func").LspAttach,
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
