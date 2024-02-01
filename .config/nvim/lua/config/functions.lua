F = {}

-- ~  --------------------------------------------------------------------------------  ~ --

function F.LspAttach(client, bufnr)
  Key.LSP(client, bufnr)
  vim.api.nvim_buf_create_user_command(
    bufnr,
    "FormatLSP",
    function(_) vim.lsp.buf.format() end,
    { desc = "Format current buffer with LSP." }
  )
  if client.server_capabilities.code_lens then
    local codelens = vim.api.nvim_create_augroup("LSPCodeLens", { clear = true })
    vim.api.nvim_create_autocmd({ "BufEnter", "InsertLeave", "CursorHold" }, {
      group = codelens,
      callback = function() vim.lsp.codelens.refresh() end,
      buffer = bufnr,
    })
  end
end

-- ~  --------------------------------------------------------------------------------  ~ --

function F.GetViMode(use_mode_icons)
  local n
  local v
  local i
  local c
  if use_mode_icons then
    n = ""
    v = ""
    i = ""
    c = ""
  else
    n = "normal"
    v = "visual"
    i = "insert"
    c = "command"
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

function F.DisableBuiltin()
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

---@param mode string|table
---@param rhs string
---@param lhs string|function
---@param bopt table
---@param desc string
function F.Keymap(mode, rhs, lhs, bopt, desc)
  bopt.desc = desc
  vim.keymap.set(mode, rhs, lhs, bopt)
end

-- ~  --------------------------------------------------------------------------------  ~ --

---@param bufnr number -- Buffer number
---@return boolean
function F.IsBigBuff(bufnr)
  local max_filesize = 1024 * 1024
  local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(bufnr))
  return ok and stats and stats.size > max_filesize
end

-- ~  --------------------------------------------------------------------------------  ~ --

---@return boolean
function F.IsBufEmpty() return vim.fn.empty(vim.fn.expand("%:t")) ~= 1 end

-- ~  --------------------------------------------------------------------------------  ~ --

---@return boolean
function F.IsGitRepo()
  local filepath = vim.fn.expand("%:p:h")
  local gitdir = vim.fn.finddir(".git", filepath .. ";")
  return gitdir and #gitdir > 0 and #gitdir < #filepath
end

-- ~  --------------------------------------------------------------------------------  ~ --

function F.Inspect(el) vim.cmd("lua = print(vim.inspect(" .. el .. "))") end

-- ~  --------------------------------------------------------------------------------  ~ --

---@param devicons table -- Object `nvim-web-devicons`
---@return table<string, string> -- Filetype icon & icon color
function F.GetFtIcon(devicons)
  local full_filename = vim.api.nvim_buf_get_name(0)
  local filename = vim.fn.fnamemodify(full_filename, ":t")
  local extension = vim.fn.fnamemodify(filename, ":e")
  local ftype_icon, ftype_icon_color = devicons.get_icon_color(filename, extension, { default = true })
  return ftype_icon and { ftype_icon .. "", ftype_icon_color }
end

-- ~  --------------------------------------------------------------------------------  ~ --

return F
