-- ~  Global functions

F = {}

-- ~  --------------------------------------------------------------------------------  ~ --

F.GetViMode = function(show_icons)
  local n
  local v
  local i
  local c
  if show_icons then
    n = ""
    v = ""
    i = ""
    c = ""
  else
    n = "normal" -- ""
    v = "visual" -- ""
    i = "insert" -- ""
    c = "command" -- ""
  end
  local r = "replace"
  local s = "select"
  local t = "terminal"
  local sh = "shell"
  local p = "prompt"
  local modes = {
    ["n"] = n,
    ["no"] = n,
    ["nov"] = n,
    ["noV"] = n,
    ["no\22"] = n,
    ["niI"] = n,
    ["niR"] = n,
    ["niV"] = n,
    ["nt"] = n,
    ["ntT"] = n,
    ["v"] = v,
    ["vs"] = v,
    ["V"] = v,
    ["Vs"] = v,
    ["\22"] = v,
    ["\22s"] = v,
    ["s"] = s,
    ["S"] = s,
    ["\19"] = i,
    ["i"] = i,
    ["ic"] = i,
    ["ix"] = i,
    ["R"] = r,
    ["Rc"] = r,
    ["Rx"] = r,
    ["Rv"] = r,
    ["Rvc"] = r,
    ["Rvx"] = r,
    ["c"] = c,
    ["cv"] = c,
    ["ce"] = c,
    ["r"] = p,
    ["rm"] = p,
    ["r?"] = p,
    ["!"] = sh,
    ["t"] = t,
  }
  return modes[vim.api.nvim_get_mode().mode] or "[Unknown]"
end

-- ~ -------------------------------------------------------------------------------- ~ --

F.DisableBuiltin = function()
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

-- ~  --------------------------------------------------------------------------------  ~ --

---@param bufnr number -- Buffer number
---@return boolean
F.IsBigBuff = function(bufnr)
  local max_filesize = 1024 * 1024
  local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(bufnr))
  return ok and stats and stats.size > max_filesize
end

-- ~  --------------------------------------------------------------------------------  ~ --

---@param mapargs table
---@return table
F.KeymapArgs = function(mapargs)
  local default = { noremap = true, silent = false, desc = "" }
  return vim.tbl_deep_extend("force", default, mapargs)
end

-- ~  --------------------------------------------------------------------------------  ~ --

---@return boolean
F.IsBufEmpty = function() return vim.fn.empty(vim.fn.expand("%:t")) ~= 1 end

-- ~  --------------------------------------------------------------------------------  ~ --

---@return boolean
F.IsGitRepo = function()
  local filepath = vim.fn.expand("%:p:h")
  local gitdir = vim.fn.finddir(".git", filepath .. ";")
  return gitdir and #gitdir > 0 and #gitdir < #filepath
end

-- ~  --------------------------------------------------------------------------------  ~ --

F.Inspect = function(el) vim.cmd("lua = print(vim.inspect(" .. el .. "))") end

-- ~  --------------------------------------------------------------------------------  ~ --

return F
