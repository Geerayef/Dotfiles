-- ~  Order: Light -> Dark
-- ~  Cannot use 'highlight Normal guibg=#000000' for themes before '~ Dark enough'

return {
  -- ~  NightFox
  -- nightfox / duskfox / terafox / carbonfox
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

  -- ~  Oxocarbon
  {
    "nyoom-engineering/oxocarbon.nvim",
    -- lazy = false,
    -- priority = 1000,
  },

  -- ~  Neodark/er
  {
    "VDuchauffour/neodark.nvim",
    -- lazy = false,
    -- priority = 1000,
    -- config = function ()
    --   require("neodark").setup({
    --     theme_style = "neodarker"
    --   })
    -- end
  },

  -- ~  Dark enough

  -- ~  Midnight: punchy syntax
  {
    "dasupradyumna/midnight.nvim",
    lazy = false,
    priority = 1000,
  },

  -- ~  Darkrose
  {
    "water-sucks/darkrose.nvim",
    -- lazy = false,
    -- priority = 1000,
  },

}
