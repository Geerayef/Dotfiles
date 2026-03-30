vim.o.wrap = true
vim.o.textwidth = 80
vim.o.spell = true
vim.o.spelllang = "en_gb"
vim.o.spelloptions = "camel"

if string.match(vim.api.nvim_buf_get_name(0), ".md", 1) == ".md" then
  vim.lsp.enable("markdown_oxide", true)
end
