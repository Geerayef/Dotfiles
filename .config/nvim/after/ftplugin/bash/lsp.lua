local bashls = {
  on_attach = require("core.func").LSPAttach,
  filetypes = { "bash", "sh" },
  root_patterns = { ".shellcheckrc" },
  settings = { bashIde = { globPattern = "*@(.bash|.inc|.sh|.command)" } },
  cmd = { "bash-language-server", "start" },
}

require("util.lsp").start(bashls)
