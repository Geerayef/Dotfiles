local status, lualine = pcall(require, "lualine")
if not status then
    return
end

local navic_status, navic = pcall(require, "nvim-navic")
if not navic_status then
  return
end

-- Favorite themes: ayu_mirage, ayu_dark, horizon, iceberg_dark
lualine.setup {
  options = {
    icons_enabled = true,
    theme = "tokyonight",
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
    lualine_c = {
      {
        "filename",
        file_status = true,
        newfile_status = false,
        path = 4,
      },
      "location"
    },
    lualine_x = {
      "encoding",
      "fileformat",
    },
    lualine_y = {
      "progress",
    },
    lualine_z = {
    }
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {},
    lualine_x = {},
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {
    lualine_a = {
      {
        "tabs",
        mode = 1,
        max_length = vim.o.columns,
      }
    },
    lualine_b = {},
    lualine_c = {},
    lualine_x = {},
    lualine_y = {
      {
        "navic",
        -- function()
        --   return navic.get_location()
        -- end,
        -- cond = function()
        --   return navic.is_available()
        -- end,
        color_correction = "dynamic",
        navic_opts = {
          separator = " > ",
          depth_limit = 5,
          depth_limit_indicator = "..",
        }
      },
    },
    lualine_z = {
    }
  },
  winbar = {},
  inactive_winbar = {},
  extensions = {}
}
