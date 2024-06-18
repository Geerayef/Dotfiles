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

if lsp.start(svelte) ~= nil then
  F.Notify("INFO", svelte.cmd[1] .. " LSP started.")
else
  F.Notify("ERROR", "Could not start LSP " .. svelte.cmd[1])
end

if lsp.start(biome) ~= nil then
  F.Notify("INFO", biome.cmd[1] .. " LSP started.")
else
  F.Notify("ERROR", "Could not start LSP " .. biome.cmd[1])
end
