local lsp = require("util.lsp")
local clangd = {
  filetypes = { "c", "h", "cpp", "chh" },
  root_patterns = { "*.clangd" },
  cmd = { "clangd" },
}
lsp.start(clangd)
