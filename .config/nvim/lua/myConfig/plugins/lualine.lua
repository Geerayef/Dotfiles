local status, lualine = pcall(require, "lualine")
if not status then
    return
end

-- Favorite themes: ayu_mirage, ayu_dark, horizon, iceberg_dark
lualine.setup {
  options = {
    icons_enabled = true,
    theme = "ayu_dark",
    component_separators = { left = "", right = "" },
    section_separators ="",
    disabled_filetypes = {
      statusline = {},
      winbar = {},
    },
    ignore_focus = {},
    always_divide_middle = true,
    globalstatus = true,
    refresh = {
      statusline = 1000,
      tabline = 1000,
      winbar = 1000,
    }
  },
  sections = {
    lualine_a = {"mode"},
    lualine_b = {
      "branch",
      "diff",
      {
        "diagnostics",
        update_in_insert = true,
      }
    },
    lualine_c = {"filename"},
    lualine_x = {
      "encoding",
      "fileformat",
      "filetype"
    },
    lualine_y = {
      "progress",
      "searchcount"
    },
    lualine_z = {}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {"filename"},
    lualine_x = {"location"},
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {
    -- lualine_a = {},
    -- lualine_b = {},
    -- lualine_c = {},
    -- lualine_x = {},
    -- lualine_y = {},
    -- lualine_z = {}
  },
  winbar = {},
  inactive_winbar = {},
  extensions = {}
}
