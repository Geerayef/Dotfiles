local api = vim.api
local autocmd = vim.api.nvim_create_autocmd

--------------------------------------------------------------------------------

-- ~ Bash LS (NOT NEEDED: using Mason)
-- autocmd("FileType", {
--   pattern = 'sh',
--   callback = function()
--     vim.lsp.start({
--         name = "bash-language-server",
--         cmd = { 
--           "/home/tibor/.local/share/nvim/mason/bin/bash-language-server",
--           "start"
--         },
--     })
--   end,
-- })

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

-- Furction definitions:

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

return M
