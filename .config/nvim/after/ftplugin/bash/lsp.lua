local lsp = require("util.lsp")
local bashls = {
  filetypes = { "sh", "bash" },
  root_patterns = { ".shellcheckrc" },
  settings = {
    bashIde = { globPattern = "*@(.sh|.inc|.bash|.command)" },
  },
  cmd = { "bash-language-server", "start" },
}

if lsp.start(bashls) ~= nil then
  F.Notify("INFO", bashls.cmd[1] .. " LSP started.")
else
  F.Notify("ERROR", "Could not start LSP " .. bashls.cmd[1])
end
