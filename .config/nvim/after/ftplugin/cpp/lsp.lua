local lsp = require("util.lsp")
local clangd = {
  filetypes = { "c", "h", "cpp", "chh" },
  root_patterns = { "*.clangd" },
  cmd = { "clangd" },
}

if lsp.start(clangd) ~= nil then
  F.Notify("INFO", clangd.cmd[1] .. " LSP started.")
else
  F.Notify("ERROR", "Could not start LSP " .. clangd.cmd[1])
end
