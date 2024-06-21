local bashls = {
  on_attach = require("core.func").LspAttach,
  filetypes = { "sh", "bash" },
  root_patterns = { ".shellcheckrc" },
  settings = {
    bashIde = { globPattern = "*@(.sh|.inc|.bash|.command)" },
  },
  cmd = { "bash-language-server", "start" },
}

require("util.lsp").start(bashls)
