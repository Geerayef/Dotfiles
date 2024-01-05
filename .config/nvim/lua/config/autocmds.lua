local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

-- ~  Highlight on yank

local hl_group = augroup("YankHighlight", { clear = true })
autocmd("TextYankPost", {
  callback = function() vim.highlight.on_yank() end,
  group = hl_group,
  pattern = "*",
})

