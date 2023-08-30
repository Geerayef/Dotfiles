return {
  -- ~  NightFox
  -- nightfox / duskfox / nordfox / terafox / carbonfox
  {
    "EdenEast/nightfox.nvim",
    lazy = true,
    opts = {
      options = {
        styles = {
          constants = "bold"
        },
        inverse = {
          match_paren = false,
          visual = true,
          search = true,
        },
        colorblind = {
          enable = true,
          severity = {
            deutan = 1
          }
        }
      }
    }
  },

  -- ~  TokyoNight
  {
    "folke/tokyonight.nvim",
    lazy = true,
    opts = {
      style = "moon",
      lualine_bold = true,
      styles = {
        keywords = { italic = false },
      },
    },
  },

  -- ~  Nord
  {
    "shaunsingh/nord.nvim",
    lazy = true,
    -- vim.g.nord_borders = true,
    -- vim.g.nord_italic = false
  },
}
