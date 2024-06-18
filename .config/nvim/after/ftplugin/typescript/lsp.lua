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

if lsp.start(tsserver) ~= nil then
  F.Notify("INFO", tsserver.cmd[1] .. " LSP started.")
else
  F.Notify("ERROR", "Could not start LSP " .. tsserver.cmd[1])
end

if lsp.start(biome) ~= nil then
  F.Notify("INFO", biome.cmd[1] .. " LSP started.")
else
  F.Notify("ERROR", "Could not start LSP " .. biome.cmd[1])
end
