local lsp = require("util.lsp")
local tsserver = {
  init_options = {
    preferences = { disableSuggestions = true },
    hostInfo = "neovim",
  },
  filetypes = { "javascript", "typescript" },
  root_patterns = { "package.json", "jsconfig.json", "tsconfig.json" },
  cmd = { "typescript-language-server", "--stdio" },
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
lsp.start(tsserver)
lsp.start(biome)
