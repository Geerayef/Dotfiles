return {
  -- ~  NightFox
  -- nightfox / duskfox / nordfox / terafox / carbonfox
  {
    "EdenEast/nightfox.nvim",
    -- lazy = false,
    -- priority = 1000,
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
            deutan = 1,
          }
        }
      }
    }
  },

  -- ~  TokyoNight
  {
    "folke/tokyonight.nvim",
    -- lazy = false,
    -- priority = 1000,
    opts = {
      style = "night",
      lualine_bold = true,
      styles = {
        keywords = { italic = false },
      },
    },
  },

  -- ~  Darkrose
  {
    "water-sucks/darkrose.nvim",
    lazy = false,
    priority = 1000,
  },

  -- ~  Midnight
  { "dasupradyumna/midnight.nvim",
    -- lazy = false,
    -- priority = 1000
  },

  -- ~  Oxocarbon
  {
    "nyoom-engineering/oxocarbon.nvim",
    -- lazy = false,
    -- priority = 1000,
  },

  -- ~  Neodark
  {
    "VDuchauffour/neodark.nvim",
    -- lazy = false,
    -- priority = 1000,
    config = function ()
      local status, neodark = pcall(require, "neodark")
      if not status then return end

      neodark.setup({
        theme_style = "neodarker"
      })
    end
  },

  -- ~  Nord
  {
    "shaunsingh/nord.nvim",
    -- lazy = false,
    -- priority = 1000,
    -- vim.g.nord_borders = true,
    -- vim.g.nord_italic = false
  },

}
