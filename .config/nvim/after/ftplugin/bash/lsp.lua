local lsp = require("util.lsp")
local bashls = {
  on_attach = require("core.func").LspAttach,
  filetypes = { "sh", "bash" },
  root_patterns = { ".shellcheckrc" },
  settings = {
    bashIde = { globPattern = "*@(.sh|.inc|.bash|.command)" },
  },
  cmd = { "bash-language-server", "start" },
}
lsp.start(bashls)
