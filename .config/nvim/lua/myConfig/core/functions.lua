local api = vim.api
local autocmd = vim.api.nvim_create_autocmd

--------------------------------------------------------------------------------

-- ~ Highlight on yank
local highlight_group = vim.api.nvim_create_augroup("YankHighlight", { clear = true })
autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = "*",
})

--------------------------------------------------------------------------------

-- #  ~ Custom, callable functions

M = {}

-- Function definitions:

M.NvimMode = function()
  local mode_map = {
    ['n'] = 'NORMAL',
    ['i'] = 'INSERT',
    ['R'] = 'REPLACE',
    ['v'] = 'VISUAL',
    ['V'] = 'V-LINE',
    ['^V'] = 'V-BLOCK',
    ['c'] = 'COMMAND',
    ['s'] = 'SELECT',
    ['S'] = 'S-LINE',
    ['^S'] = 'S-BLOCK',
    ['t'] = 'TERMINAL',
  }

  return mode_map[api.nvim_get_mode().mode] or '[Unknown]'
end

M.disable_builtin = function ()
  vim.g.loaded_netrw = 1
  vim.g.loaded_netrwPlugin = 1
  vim.g.loaded_netrwSettings = 1
  vim.g.loaded_netrwFileHandlers = 1

  vim.g.loaded_gzip = 1
  vim.g.loaded_zip = 1
  vim.g.loaded_zipPlugin = 1
  vim.g.loaded_tar = 1
  vim.g.loaded_tarPlugin = 1

  vim.g.loaded_getscript = 1
  vim.g.loaded_getscriptPlugin = 1
  vim.g.loaded_vimball = 1
  vim.g.loaded_vimballPlugin = 1
  vim.g.loaded_2html_plugin = 1

  vim.g.loaded_matchit = 1
  vim.g.loaded_logiPat = 1
  vim.g.loaded_rrhelper = 1
end

return M
