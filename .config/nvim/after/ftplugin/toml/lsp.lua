local taplo = {
  on_attach = require("core.func").LSPAttach,
  filetypes = { "toml" },
  root_patterns = { "keymap.toml", "yazi.toml", "*.toml" },
  cmd = { "taplo", "lsp", "stdio" },
}

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() require("util.lsp").start(taplo) end
  )
end)
