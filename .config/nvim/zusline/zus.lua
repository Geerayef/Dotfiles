-- #  Zus statusline

Zus = {}

-- ~ -------------------------------------------------------------------------------- ~ --

-- ~  Imports


-- ~ -------------------------------------------------------------------------------- ~ --

-- ~  Options

Zus.options = {
  icons = {
    git = { git_branch = " " },
    diagnostics = {
      error = "  ",
      warn  = "  ",
      hint  = "  ",
      info  = "  ",
    },
  },
}

-- ~ -------------------------------------------------------------------------------- ~ --

-- ~  Highlight, color, assets & icons

Zus.HIGHLIGHT = {
  ZusHL               = "%#ZusHL#",
  -- ZusHLMode           = "%#ZusHLMode#",
  -- ZusHLNormal         = "%#ZusHLNormal#",
  -- ZusHLVisual         = "%#ZusHLVisual#",
  -- ZusHLInsert         = "%#ZusHLInsert#",
  -- ZusHLCommand        = "%#ZusHLCommand#",
  -- ZusHLSelect         = "%#ZusHLVisual#",
  -- ZusHLReplace        = "%#ZusHLVisual#",
  -- ZusHLTerminal       = "%#ZusHLVisual#",
  -- ZusLSPError         = "%#ZusLSPError#",
  -- ZusLSPWarn          = "%#ZusLSPWarn#",
  -- ZusLSPHint          = "%#ZusLSPHint#",
  -- ZusLSPInfo          = "%#ZusLSPInfo#",
}

-- ~ -------------------------------------------------------------------------------- ~ --

-- ~  Helper functions

local get_diagnostic_stats = function()
  local error_count, warn_count, info_count, hint_count
  local diagnostics = vim.diagnostic.get(0)
  local count = { 0, 0, 0, 0 }
  for _, diagnostic in ipairs(diagnostics) do
    if vim.startswith(vim.diagnostic.get_namespace(diagnostic.namespace).name, 'vim.lsp') then
      count[diagnostic.severity] = count[diagnostic.severity] + 1
    end
  end
  error_count = count[vim.diagnostic.severity.ERROR]
  warn_count = count[vim.diagnostic.severity.WARN]
  info_count = count[vim.diagnostic.severity.INFO]
  hint_count = count[vim.diagnostic.severity.HINT]
  return error_count, warn_count, info_count, hint_count
end

_G.Diagnostics = function()
  local zdi = Zus.options.icons.diagnostics
  local error, warn, info, hint = get_diagnostic_stats()
  local result = (error > 0 and zdi.error .. error or "")
  .. (warn > 0 and zdi.warn .. warn or "")
  .. (hint > 0 and zdi.hint .. hint or "")
  .. (info > 0 and zdi.info .. info or "")
  if #result == 0 then
    return " "
  end
  return result
end

local path_branches = {}
_G.Git_branch = function(path)
  path = vim.fs.normalize(path or vim.api.nvim_buf_get_name(0))
  local dir = vim.fs.dirname(path)
  if not dir then return '' end
  local branch = path_branches[dir]
  if branch then return Zus.options.icons.git.git_branch .. branch end
  vim.fn.system( { 'git', '-C', dir, 'rev-parse', '--abbrev-ref', 'HEAD' },
    { stderr = false },
    function(err, _) path_branches[dir] = err.stdout:gsub('\n.*', '') end
  )
  return ''

  -- local status_fugitive = vim.g.FugitiveStatusline()
  -- local branch_name = string.match(status_fugitive, "%(([a-zA-Z0-9]+)%)")
  -- if branch_name == nil then return " " end
  -- return Zus.options.icons.git.git_branch .. branch_name
end

_G.Git_status = function()
  local status = vim.b.gitsigns_status
  if status == nil then
    return " "
  elseif status == "" then
    return " "
  end
  return "[" .. status .. "]"
end

-- ~ -------------------------------------------------------------------------------- ~ --

-- ~  Components

Zus.components = {}
local zc = Zus.components

zc.start_spacing   = "| "
zc.end_spacing     = " |"
zc.mode            = "%8.{%v:lua.F.NvimMode()%}"
zc.file_name       = "%-24.t"
zc.file_status     = "%-8.(%-3.m %-4.r%)"
zc.git_branch      = "%8.{%v:lua.Git_branch()%}"
zc.git_status      = "%12.{%v:lua.Git_status()%}"
zc.lsp_diagnostics = "%16.{%v:lua.Diagnostics()%}"

-- ~ -------------------------------------------------------------------------------- ~ --

-- ~  Sections

Zus.sections = {}
local zs = Zus.sections

zs.left  = "%(" .. zc.start_spacing .. zc.file_name .. zc.file_status .. "%)"
zs.mid   = "%=%(" .. zc.mode .. "%<" .. "%)"
zs.right = "%=%(" .. zc.git_branch .. zc.git_status .. zc.lsp_diagnostics  .. "%)" .. zc.end_spacing

-- ~ -------------------------------------------------------------------------------- ~ --

-- ~  Setup

Zus.setup = function (user_options)
  Zus.options = vim.tbl_extend("force", Zus.options, user_options)

  vim.opt.statusline = string.format("%s%s%s%s",
    Zus.HIGHLIGHT.ZusHL,
    zs.left,
    zs.mid,
    zs.right
  )

  vim.cmd("highlight link ZusHL Normal")
end

return Zus
