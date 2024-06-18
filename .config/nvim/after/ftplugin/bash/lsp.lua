local lsp = require("util.lsp")
local bashls = {
  filetypes = { "sh", "bash" },
  root_patterns = { ".shellcheckrc" },
  settings = {
    bashIde = { globPattern = "*@(.sh|.inc|.bash|.command)" },
  },
  cmd = { "bash-language-server", "start" },
}

lsp.start(bashls)
