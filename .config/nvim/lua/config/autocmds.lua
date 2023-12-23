local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

-- ~  Highlight on yank

local hl_group = augroup("YankHighlight", { clear = true })
autocmd("TextYankPost", {
  callback = function() vim.highlight.on_yank() end,
  group = hl_group,
  pattern = "*",
})

-- ~  Heirline

local status, Util = pcall(require, "heirline.utils")
if status then
  augroup("Heirline", { clear = true })
  vim.cmd([[au Heirline FileType * if index(['wipe', 'delete'], &bufhidden) >= 0 | set nobuflisted | endif]])
  autocmd("ColorScheme", {
    callback = function() Util.on_colorscheme(F.heirline_get_highlight_colors(Util)) end,
    group = "Heirline",
  })
end
