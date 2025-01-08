local bashls = {
  on_attach = require("core.func").LSPAttach,
  filetypes = { "bash", "sh" },
  root_patterns = { ".shellcheckrc" },
  settings = { bashIde = { globPattern = "*@(.bash|.inc|.sh|.command)" } },
  cmd = { "bash-language-server", "start" },
}

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() require("util.lsp").start(bashls) end
  )
end)
