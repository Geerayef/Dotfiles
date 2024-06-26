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
          NormalDark = { fg = t.ui.fg_dim, bg = t.ui.bg_ayu },
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
  {
    "sainnhe/gruvbox-material",
    init = function()
      vim.g.gruvbox_material_background = "hard"
      vim.g.gruvbox_material_foreground = "material"
      vim.g.gruvbox_material_enable_italic = 1
      vim.g.gruvbox_material_ui_contrast = "high"
      vim.g.gruvbox_material_statusline_style = "mix"
      vim.g.gruvbox_material_float_style = "dim"
      vim.g.gruvbox_material_better_performancs = 1
      vim.g.gruvbox_material_colors_override = { ["bg0"] = "#0A0E14" }
    end,
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
