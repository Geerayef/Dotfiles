-- ~  Global functions

F = {}

-- ~ -------------------------------------------------------------------------------- ~ --

F.NvimMode = function()
  local n = "normal"  -- ""
  local v = "visual"  -- ""
  local i = "insert"  -- ""
  local c = "command" -- ""
  local r = "replace"
  local s = "select"
  local t = "terminal"
  local modes = {
    ["n"]     = n,
    ["no"]    = n,
    ["nov"]   = n,
    ["noV"]   = n,
    ["no\22"] = n,
    ["niI"]   = n,
    ["niR"]   = n,
    ["niV"]   = n,
    ["nt"]    = n,
    ["ntT"]   = n,
    ["v"]     = v,
    ["vs"]    = v,
    ["V"]     = v,
    ["Vs"]    = v,
    ["\22"]   = v,
    ["\22s"]  = v,
    ["s"]     = s,
    ["S"]     = s,
    ["\19"]   = i,
    ["i"]     = i,
    ["ic"]    = i,
    ["ix"]    = i,
    ["R"]     = r,
    ["Rc"]    = r,
    ["Rx"]    = r,
    ["Rv"]    = r,
    ["Rvc"]   = r,
    ["Rvx"]   = r,
    ["c"]     = c,
    ["cv"]    = c,
    ["ce"]    = c,
    ["r"]     = r,
    ["rm"]    = r,
    ["r?"]    = "?",
    ["!"]     = "󰩌",
    ["t"]     = t,
  }
  return modes[vim.api.nvim_get_mode().mode] or "[Unknown]"
end

-- ~ -------------------------------------------------------------------------------- ~ --

F.disable_builtin = function ()
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
  g.loaded_sql_completion = 1

  g.loaded_perl_provider = 0
  g.loaded_ruby_provider = 0
  g.loaded_node_provider = 0
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

-- ~ -------------------------------------------------------------------------------- ~ --

F.heirline_get_highlight_colors = function (util)
  return {
    bright_bg = util.get_highlight("Folded").bg,
    bright_fg = util.get_highlight("Folded").fg,
    dark_red = util.get_highlight("DiffDelete").bg,
    red = util.get_highlight("DiagnosticError").fg,
    green = util.get_highlight("String").fg,
    blue = util.get_highlight("Function").fg,
    gray = util.get_highlight("NonText").fg,
    orange = util.get_highlight("Constant").fg,
    purple = util.get_highlight("Statement").fg,
    cyan = util.get_highlight("Special").fg,
    diag_warn = util.get_highlight("DiagnosticWarn").fg,
    diag_error = util.get_highlight("DiagnosticError").fg,
    diag_hint = util.get_highlight("DiagnosticHint").fg,
    diag_info = util.get_highlight("DiagnosticInfo").fg,
    git_del = util.get_highlight("diffDeleted").fg,
    git_add = util.get_highlight("diffAdded").fg,
    git_change = util.get_highlight("diffChanged").fg,
  }
end

return F
