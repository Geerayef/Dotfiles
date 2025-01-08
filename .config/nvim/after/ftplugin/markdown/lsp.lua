local markdown_oxide = {
  on_attach = require("core.func").LSPAttach,
  filetypes = { "markdown" },
  root_patterns = { "*.md", ".obsidian/", ".moxide.toml" },
  cmd = { "markdown-oxide" },
}

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() require("util.lsp").start(markdown_oxide) end
  )
end)
