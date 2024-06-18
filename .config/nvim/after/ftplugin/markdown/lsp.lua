local lsp = require("util.lsp")
local markdown_oxide = {
  filetypes = { "markdown" },
  root_patterns = { "*.md" },
  cmd = { "markdown-oxide" },
}

if lsp.start(markdown_oxide) ~= nil then
  F.Notify("INFO", markdown_oxide.cmd[1] .. " LSP started.")
else
  F.Notify("ERROR", "Could not start LSP " .. markdown_oxide.cmd[1])
end
