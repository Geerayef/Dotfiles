-- ~  Functions used globally

F = {}

-- ~ -------------------------------------------------------------------------------- ~ --

F.NvimMode = function()
  local n = 'normal'  -- ''
  local v = 'visual'  -- ''
  local i = 'insert'  -- ''
  local c = 'command' -- ''
  local r = 'replace'
  local s = 'select'
  local t = 'terminal'
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
  return modes[vim.api.nvim_get_mode().mode] or '[Unknown]'
end

-- ~ -------------------------------------------------------------------------------- ~ --

F.disable_builtin = function ()
  local vim = vim
  local g = vim.g
  g.loaded_netrw = 1
  g.loaded_netrwPlugin = 1
  g.loaded_netrwSettings = 1
  g.loaded_netrwFileHandlers = 1

  g.loaded_gzip = 1
  g.loaded_zip = 1
  g.loaded_zipPlugin = 1
  g.loaded_tar = 1
  g.loaded_tarPlugin = 1

  g.loaded_getscript = 1
  g.loaded_getscriptPlugin = 1
  g.loaded_vimball = 1
  g.loaded_vimballPlugin = 1
  g.loaded_2html_plugin = 1

  g.loaded_matchit = 1
  g.loaded_matchparen = 1
  g.loaded_logiPat = 1
  g.loaded_rrhelper = 1
end

-- ~ -------------------------------------------------------------------------------- ~ --

F.IsBigBuff = function(bufnr)
  -- 100 KB
  local max_filesize = 100 * 1024
  local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(bufnr))
  if ok and stats and stats.size > max_filesize then
    return true
  else
    return false
  end
end

return F
