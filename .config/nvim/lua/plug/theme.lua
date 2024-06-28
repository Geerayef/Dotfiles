return {
  {
    "rebelot/kanagawa.nvim",
    lazy = false,
    priority = 1010,
    opts = {
      compile = true,
      functionStyle = { italic = true, bold = true },
      typeStyle = { italic = true, bold = true },
      keywordStyle = { bold = true },
      statementStyle = {},
      overrides = function(colors)
        local t = colors.theme
        return {
          NormalDark = { fg = t.ui.fg_dim, bg = "#0A0E14" },
          FloatTitle = { bg = "none" },
          FloatBorder = { bg = "none" },
          NormalFloat = { bg = "none" },
          MasonNormal = { bg = "none", fg = t.ui.fg_dim },
          LazyNormal = { bg = "none", fg = t.ui.fg_dim },
          TelescopeTitle = { fg = t.ui.special, bg = "none", bold = true },
          TelescopePromptBorder = { fg = t.ui.fg, bg = "none" },
          TelescopeResultsBorder = { fg = t.ui.fg, bg = "none" },
          TelescopePreviewBorder = { fg = t.ui.fg, bg = "none" },
        }
      end,
      colors = {
        theme = { all = { ui = { bg = "#0A0E14", bg_gutter = "none" } } },
      },
      theme = "dragon",
      background = { dark = "dragon", light = "wave" },
    },
  },
  -- {
  --   "marko-cerovac/material.nvim",
  --   lazy = false,
  --   priority = 1010,
  --   init = function() vim.g.material_style = "deep ocean" end,
  --   opts = {
  --     contrast = { floating_windows = true, lsp_virtual_text = true },
  --     styles = {
  --       comments = { italic = true },
  --       functions = { italic = true, bold = true },
  --       types = { italic = true, bold = true },
  --     },
  --     plugins = {
  --       "flash",
  --       "gitsigns",
  --       "indent-blankline",
  --       "neogit",
  --       "noice",
  --       "nvim-notify",
  --       "nvim-cmp",
  --       "nvim-web-devicons",
  --     },
  --     high_visibility = { lighter = true, darker = true },
  --     lualine_style = "stealth",
  --     custom_colors = nil,
  --     custom_highlights = {},
  --   },
  -- },
}
