local tsserver = {
  on_attach = require("core.func").LspAttach,
  init_options = {
    preferences = { disableSuggestions = true },
    hostInfo = "neovim",
  },
  filetypes = { "javascript", "typescript" },
  root_patterns = { "package.json", "jsconfig.json", "tsconfig.json" },
  cmd = { "typescript-language-server", "--stdio" },
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

require("util.lsp").start(tsserver)
require("util.lsp").start(biome)
