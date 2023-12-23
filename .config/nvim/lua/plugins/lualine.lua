local kanagawa = require("kanagawa.colors").setup({ theme = "dragon" })
local palette = kanagawa.palette
local theme = kanagawa.theme

local conditions = {
  buffer_not_empty = function() return vim.fn.empty(vim.fn.expand("%:t")) ~= 1 end,
  hide_in_width = function() return vim.fn.winwidth(0) > 80 end,
  check_git_workspace = function()
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

-- Config

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
    lualine_y = {},
    lualine_z = {},
    lualine_c = {},
    lualine_x = {},
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_y = { "filename" },
    lualine_z = { "location" },
    lualine_c = {},
    lualine_x = {},
  },
}

local insert_c = function (component) table.insert(config.sections.lualine_c, component) end

local insert_x = function (component) table.insert(config.sections.lualine_x, component) end

-- ~  --------------------------------------------------------------------------------  ~ --

-- ~  Left

insert_c({ function() return "| " end, color = { fg = palette.dragonWhite }, padding = { left = 0 } })

insert_c({
  function() return "" end,
  color = function() return { fg = mode_colors[vim.fn.mode()] } end,
  padding = { right = 1 },
})

insert_c({ "filename", cond = conditions.buffer_not_empty, color = { fg = palette.dragonAqua } })

-- ~  --------------------------------------------------------------------------------  ~ --

-- ~  Mid

insert_c({ function() return "%=" end })

-- insert_c({
--   function () return "%8.(" .. F.NvimMode() .. "%)" end,
--   color = function () return { fg = mode_colors[vim.fn.mode()] } end,
-- })

insert_c({ function() return "%=" end })

-- ~  --------------------------------------------------------------------------------  ~ --

-- ~  Right

insert_x({
  "diff",
  source = function ()
    local gitsigns = vim.b.gitsigns_status_dict
    if gitsigns then return {
        added = gitsigns.added,
        modified = gitsigns.changed,
        removed = gitsigns.removed
      }
    end
  end,
  symbols = { added = " ", modified = "~ ", removed = " " },
  colored = true,
  diff_color = {
    added = { fg = palette.autumnGreen },
    modified = { fg = palette.surimiOrange },
    removed = { fg = palette.samuraiRed }
  }
})

insert_x({
  "diagnostics",
  sources = { "nvim_lsp", "nvim_diagnostic" },
  symbols = { error = " ", warn = " ", info = " ", hint = " " },
  diagnostics_color = {
    error = { fg = palette.dragonRed },
    warn = { fg = palette.dragonYellow },
    info = { fg = palette.dragonCyan },
    hint = { fg = palette.dragonGray }
  }
})

insert_x({ "branch", icon = " ", color = { fg = palette.dragonViolet } })

insert_x({ function() return " |" end, color = { fg = "white" }, padding = { right = 0  } })

return {
  "nvim-lualine/lualine.nvim",
  config = function ()
    local has_lualine, lualine = pcall(require, "lualine")
    if not has_lualine then return end
    lualine.setup(config)
  end
}

