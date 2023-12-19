return {
  -- ~  NightFox
  -- {
  --   "EdenEast/nightfox.nvim",
  --   lazy = false,
  --   priority = 1000,
  --   opts = {
  --     options = {
  --       styles = { constants = "bold" },
  --       inverse = { match_paren = false, visual = true, search = true },
  --       colorblind = { enable = true, severity = { deutan = 1 } }
  --     }
  --   }
  -- },

  -- ~  TokyoNight
  -- {
  --   "folke/tokyonight.nvim",
  --   lazy = false,
  --   priority = 1000,
  --   opts = {
  --     style = "night",
  --     lualine_bold = true,
  --     styles = { keywords = { italic = false } },
  --   },
  -- },

  -- ~  Kanagawa
  {
    "rebelot/kanagawa.nvim",
    lazy = false,
    priority = 1000,
    config = function ()
      require("kanagawa").setup({
        compile = true,
        undercurl = true,
        commentStyle = { italic = true },
        functionStyle = {},
        keywordStyle = { italic = true},
        statementStyle = { bold = true },
        typeStyle = {},
        transparent = false,
        dimInactive = false,
        terminalColors = true,
        overrides = function(colors)
          local theme = colors.theme
          return {
            NormalFloat = { bg = "none" },
            FloatBorder = { bg = "none" },
            FloatTitle = { bg = "none" },
            NormalDark = { fg = theme.ui.fg_dim, bg = theme.ui.bg_m3 },
            LazyNormal = { bg = theme.ui.bg_m3, fg = theme.ui.fg_dim },
            MasonNormal = { bg = theme.ui.bg_m3, fg = theme.ui.fg_dim },
            TelescopeTitle = { fg = theme.ui.special, bold = true },
            TelescopePromptNormal = { bg = theme.ui.bg_p1 },
            TelescopePromptBorder = { fg = theme.ui.bg_p1, bg = theme.ui.bg_p1 },
            TelescopeResultsNormal = { fg = theme.ui.fg_dim, bg = theme.ui.bg_m1 },
            TelescopeResultsBorder = { fg = theme.ui.bg_m1, bg = theme.ui.bg_m1 },
            TelescopePreviewNormal = { bg = theme.ui.bg_dim },
            TelescopePreviewBorder = { bg = theme.ui.bg_dim, fg = theme.ui.bg_dim },
          }
        end,
        colors = { theme = { all = { ui = { bg_gutter = "none" } } } },
        theme = "dragon",
        background = { dark = "dragon", light = "lotus" },
      })
    end
  },

  -- ~  Oxocarbon
  -- {
  --   "nyoom-engineering/oxocarbon.nvim",
  --   lazy = false,
  --   priority = 1000,
  -- },

  -- ~  Neodark/er
  -- {
  --   "VDuchauffour/neodark.nvim",
  --   lazy = false,
  --   priority = 1000,
  --   opts = { theme_style = "neodarker" }
  -- },

  -- ~  Midnight
  -- {
  --   "dasupradyumna/midnight.nvim",
  --   lazy = false,
  --   priority = 1000,
  -- },
}
