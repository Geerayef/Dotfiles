vim.o.wrap = true
vim.o.textwidth = 80
vim.o.spell = true
vim.o.spelllang = "en_gb"
vim.o.spelloptions = "camel"

---@type vim.lsp.Config
local markdown_oxide = {
  name = "Markdown Oxide LS",
  cmd = { "markdown-oxide" },
  on_attach = LSP.attach,
  filetypes = { "markdown" },
  root_markers = { "*.md", ".obsidian/", ".moxide.toml" },
}

if string.match(vim.api.nvim_buf_get_name(0), ".md", 1) == ".md" then
  vim.schedule(function()
    vim.api.nvim_win_call(
      vim.api.nvim_get_current_win(),
      function() LSP.start(markdown_oxide) end
    )
  end)
end
