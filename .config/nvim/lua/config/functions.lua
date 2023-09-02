local api = vim.api

-- #  ~ Custom, callable functions

F = {}

F.NvimMode = function()
  local n = 'NORMAL'  -- ''
  local v = 'VISUAL'  -- ''
  local i = 'INSERT'  -- ''
  local c = 'COMMAND' -- ''
  local r = 'REPLACE'
  local s = 'SELECT'
  local t = 'TERMINAL'
  local modes = {
      ['n']     = n,
      ['no']    = n,
      ['nov']   = n,
      ['noV']   = n,
      ['no\22'] = n,
      ['niI']   = n,
      ['niR']   = n,
      ['niV']   = n,
      ['nt']    = n,
      ['ntT']   = n,
      ['v']     = v,
      ['vs']    = v,
      ['V']     = v,
      ['Vs']    = v,
      ['\22']   = v,
      ['\22s']  = v,
      ['s']     = s,
      ['S']     = s,
      ['\19']   = i,
      ['i']     = i,
      ['ic']    = i,
      ['ix']    = i,
      ['R']     = r,
      ['Rc']    = r,
      ['Rx']    = r,
      ['Rv']    = r,
      ['Rvc']   = r,
      ['Rvx']   = r,
      ['c']     = c,
      ['cv']    = c,
      ['ce']    = c,
      ['r']     = r,
      ['!']     = '󰩌',
      ['t']     = t,
  }
  return modes[vim.fn.mode()] or '[Unknown]'
end

F.disable_builtin = function ()
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

return F
