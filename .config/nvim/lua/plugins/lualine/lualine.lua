local kanagawa = require("kanagawa.colors").setup({ theme = "dragon" })
local palette = kanagawa.palette
local theme = kanagawa.theme
local Icons = require("nvim-web-devicons")

local static = {}

local get_icon = function ()
  local full_filename = vim.api.nvim_buf_get_name(0)
  local filename = vim.fn.fnamemodify(full_filename, ":t")
  local extension = vim.fn.fnamemodify(filename, ":e")
  static.ftype_icon, static.ftype_icon_color = Icons.get_icon_color(filename, extension, { default = true })
  return static.ftype_icon and static.ftype_icon .. ""
end

local condition = {
  is_buf_empty = function() return vim.fn.empty(vim.fn.expand("%:t")) ~= 1 end,
  win_width_ge = function(width) return vim.fn.winwidth(0) > width end,
  is_git_repo = function()
    local filepath = vim.fn.expand("%:p:h")
    local gitdir = vim.fn.finddir(".git", filepath .. ";")
    return gitdir and #gitdir > 0 and #gitdir < #filepath
  end,
}

local mode_colors = {
  n      = palette.dragonRed,
  no     = palette.dragonRed,
  cv     = palette.dragonRed,
  ce     = palette.dragonRed,
  ["!"]  = palette.dragonRed,
  t      = palette.dragonRed,
  i      = palette.dragonGreen,
  v      = palette.dragonBlue,
  [""] = palette.dragonBlue,
  V      = palette.dragonBlue,
  c      = palette.dragonAqua,
  s      = palette.dragonOrange,
  S      = palette.dragonOrange,
  [""] = palette.dragonOrange,
  ic     = palette.dragonYellow,
  R      = palette.dragonViolet,
  Rv     = palette.dragonViolet,
  r      = palette.dragonTeal,
  rm     = palette.dragonTeal,
  ["r?"] = palette.dragonTeal,
}

-- ~  --------------------------------------------------------------------------------  ~ --

-- ~  Config

local config = {
  options = {
    component_separators = "",
    section_separators = "",
    always_divide_middle = true,
    theme = {
      normal = { c = { fg = theme.ui.fg, bg = "Normal", gui = "bold" } },
      inactive = { c = { fg = theme.ui.fg_dim, bg = theme.ui.bg_m3 } },
    },
  },
  sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {},
    lualine_x = {},
    lualine_y = {},
    lualine_z = {}
  },
  inactive_sections = {
    lualine_a = { "filename" },
    lualine_b = { "location" },
    lualine_c = {},
    lualine_x = {},
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {},
    lualine_x = {},
    lualine_y = {},
    lualine_z = {}
  }
}

local status_c = function (component) table.insert(config.sections.lualine_c, component) end
local status_x = function (component) table.insert(config.sections.lualine_x, component) end
local tab_c = function (component) table.insert(config.tabline.lualine_c, component) end
local tab_x = function (component) table.insert(config.tabline.lualine_x, component) end

-- ~  --------------------------------------------------------------------------------  ~ --

-- ~  Status line
-- ~  Left

status_c({ function() return "| " end, color = { fg = palette.dragonWhite }, padding = { left = 0 } })

status_c({
  function() return O.Icons.mode end,
  color = function() return { fg = mode_colors[vim.fn.mode()] } end,
  padding = { right = 1 },
})

status_c({
  function() return get_icon() end,
  cond = condition.is_buf_empty,
  color = { fg = static.ftype_icon_color },
  padding = { left = 1, right = 0 }
})

status_c({
  "filename",
  cond = condition.is_buf_empty,
  path = 0,
  color = { fg = palette.dragonAqua },
  symbols = {
    modified = O.Icons.touched,
    readonly = O.Icons.lock,
    unnamed = "[No Name]",
    newfile = "[New]"
  }
})

-- ~  --------------------------------------------------------------------------------  ~ --
-- ~  Mid

status_c({ function() return "%=" end })

-- ~  --------------------------------------------------------------------------------  ~ --
-- ~  Right

status_x({
  "diff",
  source = function ()
    local gitsigns = vim.b.gitsigns_status_dict
    if gitsigns then
      return {
        added = gitsigns.added,
        modified = gitsigns.changed,
        removed = gitsigns.removed
      }
    end
  end,
  symbols = { added = O.Icons.added, modified = O.Icons.modified_simple, removed = O.Icons.removed },
  colored = true,
  diff_color = {
    -- palette.autumnGreen
    added = { fg = theme.vcs.added },
    -- palette.surimiOrange
    modified = { fg = theme.vcs.changed },
    -- palette.samuraiRed
    removed = { fg = theme.vcs.removed }
  }
})

status_x({
  "diagnostics",
  sources = { "nvim_lsp", "nvim_diagnostic" },
  symbols = { error = O.Icons.error, warn = O.Icons.warn, info = O.Icons.info, hint = O.Icons.hint },
  diagnostics_color = {
    error = { fg = theme.diag.error },
    warn = { fg =  theme.diag.warning },
    info = { fg =  theme.diag.info },
    hint = { fg =  theme.diag.hint }
  }
})

status_x({ "branch", icon = O.Icons.git_branch, color = { fg = palette.dragonViolet } })

status_x({ function() return " |" end, color = { fg = palette.dragonWhite }, padding = { right = 0 } })

-- ~  --------------------------------------------------------------------------------  ~ --

-- ~  Tab line

-- tab_c({
--   "tabs",
--   cond = function () vim.opt.showtabline = 1; return vim.fn.tabpagenr("$") > 1 end,
--   tab_max_length = 20,
--   max_length = vim.o.columns / 2,
--   mode = 0,
--   path = 0,
--   show_modified_status = false,
--   use_mode_colors = true,
--   symbols = { modified = " " .. O.Icons.touched },
--   padding = 1,
--   separator = "|"
-- })

tab_x({
  "buffers",
  cond = function () vim.opt.showtabline = 1; return vim.fn.tabpagenr("$") > 1 end,
  show_filename_only = true,
  hide_filename_extension = false,
  show_modified_status = false,
  mode = 1,
  max_length = vim.o.columns / 2,
  filetype_names = {
    TelescopePrompt = "Telescope",
    dashboard = "Dashboard",
    packer = "Packer",
    fzf = "FZF",
    alpha = "Alpha",
    lazy = "Lazy"
  },
  use_mode_colors = true,
  buffers_color = { active = "lualine_c_normal", inactive = "lualine_c_inactive" },
  symbols = {
    modified = " ●",
    alternate_file = "#",
    directory =  "",
  },
})

-- ~  --------------------------------------------------------------------------------  ~ --

return config
