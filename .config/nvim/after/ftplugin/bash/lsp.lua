local bashls = {
  on_attach = require("core.func").LspAttach,
  filetypes = { "bash", "sh" },
  root_patterns = { ".shellcheckrc" },
  settings = {
    bashIde = { globPattern = "*@(.bash|.inc|.sh|.command)" },
  },
  cmd = { "bash-language-server", "start" },
}

LSP.start(bashls)
