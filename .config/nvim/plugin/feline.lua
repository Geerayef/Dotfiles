local status, feline = pcall(require, "feline")
if not status then
  return
end

-- # Theme color and highligh groups # --

local fmt = string.format

-- Colors

---Convert color number to hex string
---@param n number
---@return string
local hex = function(n)
  return fmt("#%06x", n)
end

---Parse `style` string into nvim_set_hl options
---@param style string
---@return table
local function parse_style(style)
  if not style or style == "NONE" then
    return {}
  end

  local result = {}
  for token in string.gmatch(style, "([^,]+)") do
    result[token] = true
  end

  return result
end

---Get highlight opts for a given highlight group name
---@param name string
---@return table
local function get_highlight(name)
  local hl = vim.api.nvim_get_hl_by_name(name, true)
  if hl.link then
    return get_highlight(hl.link)
  end

  local result = parse_style(hl.style)
  result.fg = hl.foreground and hex(hl.foreground)
  result.bg = hl.background and hex(hl.background)
  result.sp = hl.special and hex(hl.special)

  return result
end

---Set highlight group from provided table
---@param groups table
local function set_highlights(groups)
  for group, opts in pairs(groups) do
    vim.api.nvim_set_hl(0, group, opts)
  end
end

---Generate a color palette from the current applied colorscheme
---@return table
local function generate_pallet_from_colorscheme()
  -- stylua: ignore
  local color_map = {
    black   = { index = 0, default = "#393b44" },
    red     = { index = 1, default = "#c94f6d" },
    green   = { index = 2, default = "#81b29a" },
    yellow  = { index = 3, default = "#dbc074" },
    blue    = { index = 4, default = "#719cd6" },
    magenta = { index = 5, default = "#9d79d6" },
    cyan    = { index = 6, default = "#63cdcf" },
    white   = { index = 7, default = "#dfdfe0" },
  }

  local diagnostic_map = {
    hint = { hl = "DiagnosticHint", default = color_map.green.default },
    info = { hl = "DiagnosticInfo", default = color_map.blue.default },
    warn = { hl = "DiagnosticWarn", default = color_map.yellow.default },
    error = { hl = "DiagnosticError", default = color_map.red.default },
  }

  local pallet = {}
  for name, value in pairs(color_map) do
    local global_name = "terminal_color_" .. value.index
    pallet[name] = vim.g[global_name] and vim.g[global_name] or value.default
  end

  for name, value in pairs(diagnostic_map) do
    pallet[name] = get_highlight(value.hl).fg or value.default
  end

  pallet.sl = get_highlight("StatusLine")
  pallet.sel = get_highlight("TabLineSel")

  return pallet
end

---Generate user highlight groups based on the curent applied colorscheme
---
---NOTE: This is a global because I dont known where this file will be in your config
---and it is needed for the autocmd below
_G._generate_user_statusline_highlights = function()
  local pal = generate_pallet_from_colorscheme()

  -- stylua: ignore
  local sl_colors = {
    Black   = { fg = pal.black, bg = pal.white },
    Red     = { fg = pal.red, bg = pal.sl.bg },
    Green   = { fg = pal.green, bg = pal.sl.bg },
    Yellow  = { fg = pal.yellow, bg = pal.sl.bg },
    Blue    = { fg = pal.blue, bg = pal.sl.bg },
    Magenta = { fg = pal.magenta, bg = pal.sl.bg },
    Cyan    = { fg = pal.cyan, bg = pal.sl.bg },
    White   = { fg = pal.white, bg = pal.black },
  }

  local colors = {}
  for name, value in pairs(sl_colors) do
    colors["User" .. name] = { fg = value.fg, bg = value.bg, bold = true }
    colors["UserRv" .. name] = { fg = value.bg, bg = value.fg, bold = true }
  end

  local status = vim.o.background == "dark" and { fg = pal.black, bg = pal.white } or { fg = pal.white, bg = pal.black }

  local groups = {
    -- statusline
    UserSLHint = { fg = pal.sl.bg, bg = pal.hint, bold = true },
    UserSLInfo = { fg = pal.sl.bg, bg = pal.info, bold = true },
    UserSLWarn = { fg = pal.sl.bg, bg = pal.warn, bold = true },
    UserSLError = { fg = pal.sl.bg, bg = pal.error, bold = true },
    UserSLStatus = { fg = status.fg, bg = status.bg, bold = true },

    UserSLFtHint = { fg = pal.sel.bg, bg = pal.hint },
    UserSLHintInfo = { fg = pal.hint, bg = pal.info },
    UserSLInfoWarn = { fg = pal.info, bg = pal.warn },
    UserSLWarnError = { fg = pal.warn, bg = pal.error },
    UserSLErrorStatus = { fg = pal.error, bg = status.bg },
    UserSLStatusBg = { fg = status.bg, bg = pal.sl.bg },

    UserSLAlt = pal.sel,
    UserSLAltSep = { fg = pal.sl.bg, bg = pal.sel.bg },
    UserSLGitBranch = { fg = pal.yellow, bg = pal.sl.bg },
  }

  set_highlights(vim.tbl_extend("force", colors, groups))
end

_generate_user_statusline_highlights()

vim.api.nvim_create_augroup("UserStatuslineHighlightGroups", { clear = true })
vim.api.nvim_create_autocmd({ "SessionLoadPost", "ColorScheme" }, {
  callback = function()
    _generate_user_statusline_highlights()
  end,
})

----------------------------------------------------------------------------------------------------
-- Feline

local vi = {
  -- Map vi mode to text name
  text = {
    n = "NORMAL",
    no = "NORMAL",
    i = "INSERT",
    v = "VISUAL",
    V = "V-LINE",
    [""] = "V-BLOCK",
    c = "COMMAND",
    cv = "COMMAND",
    ce = "COMMAND",
    R = "REPLACE",
    Rv = "REPLACE",
    s = "SELECT",
    S = "SELECT",
    [""] = "SELECT",
    t = "TERMINAL",
  },

  -- Maps vi mode to highlight group color defined above
  colors = {
    n = "UserRvCyan",
    no = "UserRvCyan",
    i = "UserSLStatus",
    v = "UserRvMagenta",
    V = "UserRvMagenta",
    [""] = "UserRvMagenta",
    R = "UserRvRed",
    Rv = "UserRvRed",
    r = "UserRvBlue",
    rm = "UserRvBlue",
    s = "UserRvMagenta",
    S = "UserRvMagenta",
    [""] = "FelnMagenta",
    c = "UserRvYellow",
    ["!"] = "UserRvBlue",
    t = "UserRvBlue",
  },

  -- Maps vi mode to seperator highlight goup defined above
  sep = {
    n = "UserCyan",
    no = "UserCyan",
    i = "UserSLStatusBg",
    v = "UserMagenta",
    V = "UserMagenta",
    [""] = "UserMagenta",
    R = "UserRed",
    Rv = "UserRed",
    r = "UserBlue",
    rm = "UserBlue",
    s = "UserMagenta",
    S = "UserMagenta",
    [""] = "FelnMagenta",
    c = "UserYellow",
    ["!"] = "UserBlue",
    t = "UserBlue",
  },
}

---Get highlight group from vi mode
---@return string
local function vi_mode_hl()
  return vi.colors[vim.fn.mode()] or "UserSLViBlack"
end

---Get sep highlight group from vi mode
local function vi_sep_hl()
  return vi.sep[vim.fn.mode()] or "UserSLBlack"
end

-- # -------------------------------------------------------------------------------- # --

local c = {
  vim_mode = {
    provider = {
      name = "vi_mode",
      opts = {
        show_mode_name = true,
        padding = "center", -- Uncomment for extra padding.
      },
    },
    hl = vi_mode_hl,
    left_sep = {
      str = "block",
      hl = vi_sep_hl,
    },
    right_sep = {
      str = "block",
      hl = vi_sep_hl,
    }
  },
  gitBranch = {
    provider = "git_branch",
    left_sep = "block",
    right_sep = "block",
  },
  gitDiffAdded = {
    provider = "git_diff_added",
    left_sep = "block",
    right_sep = "block",
  },
  gitDiffRemoved = {
    provider = "git_diff_removed",
    left_sep = "block",
    right_sep = "block",
  },
  gitDiffChanged = {
    provider = "git_diff_changed",
    left_sep = "block",
    right_sep = "right_filled",
  },
  separator = {
    provider = "",
    hl = "StatusLine"
  },
  fileinfo = {
    provider = {
      name = "file_info",
      opts = {
        type = "relative-short",
      },
    },
    hl = "StatusLine",
    left_sep = "block",
    right_sep = "block"
  },
  defaul = {
    provider = "",
    hl = "StatusLine"
  },
  diagnostic_errors = {
    provider = "diagnostic_errors",
    hl = "UserSLError",
    right_sep = {
      str = " ",
      hl = "UserSLWarnError",
    },
  },
  diagnostic_warnings = {
    provider = "diagnostic_warnings",
    hl = "UserSLWarn",
    right_sep = {
      str = " ",
      hl = "UserSLInfoWarn",
    },
  },
  diagnostic_hints = {
    provider = "diagnostic_hints",
    hl = "UserSLHint",
    right_sep = {
      str = " ",
      hl = "UserSLFtHint",
    },
  },
  diagnostic_info = {
    provider = "diagnostic_info",
    hl = "UserSLInfo",
    right_sep = {
      str = " ",
      hl = "UserSLHintInfo",
    },
  },
  -- file_encoding = {
  --     provider = "file_encoding",
  --     left_sep = "block",
  --     right_sep = "block",
  -- },
  position = {
    provider = {
      name = "position",
      opts = {
        format = "%4.l:%-3.c"
      }
    },
    hl = vi_mode_hl,
    left_sep = "block",
    right_sep = "block",
  },
  line_percentage = {
    provider = "line_percentage",
    hl = vi_mode_hl,
    left_sep = "block",
    right_sep = "block",
  },
}

local left = {
  c.vim_mode,
  c.gitBranch,
  c.gitDiffAdded,
  c.gitDiffRemoved,
  c.gitDiffChanged,
  c.separator,
}

local middle = {
  c.fileinfo,
  c.separator,
  c.default,
}

local right = {
  c.diagnostic_errors,
  c.diagnostic_warnings,
  c.diagnostic_info,
  c.diagnostic_hints,
  c.position,
  c.line_percentage,
  c.separator,
}

local components = {
  active = {
    left,
    middle,
    right,
  },
  inactive = {
    left,
    middle,
    right,
  },
}

feline.setup({
  components = components,
  highlight_reset_triggers = {},
  force_inactive = {
    filetypes = {
      "NvimTree",
      "packer",
      "dap-repl",
      "dapui_scopes",
      "dapui_stacks",
      "dapui_watches",
      "dapui_repl",
      "LspTrouble",
      "qf",
      "help",
    },
    buftypes = { "terminal" },
    bufnames = {},
  },
  disable = {
    filetypes = {
      "NvimTree",
      "dashboard",
      "startify",
    },
  },
})
