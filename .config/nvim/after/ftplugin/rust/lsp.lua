local lsp = require("util.lsp")
local rust_analyzer = {
  filetypes = { "rust" },
  root_patterns = { "*.rs" },
  settings = {
    ["rust-analyzer"] = {
      imports = { prefix = "self", granularity = { group = "module" } },
      cargo = { buildScripts = { enable = true } },
      procMacro = { enable = true },
    },
  },
  cmd = { "rust-analyzer" },
}

if lsp.start(rust_analyzer) ~= nil then
  F.Notify("INFO", rust_analyzer.cmd[1] .. " LSP started.")
else
  F.Notify("ERROR", "Could not start LSP " .. rust_analyzer.cmd[1])
end
