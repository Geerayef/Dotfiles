return {
  -- ~ ITALIC -------------------------------------------------------- ~ --
  {
    "rebelot/kanagawa.nvim",
    lazy = false,
    priority = 1010,
    opts = {
      compile = true,
      functionStyle = { italic = true, bold = true },
      typeStyle = { italic = true, bold = true },
      keywordStyle = {},
      statementStyle = {},
      overrides = function(colors)
        local t = colors.theme
        return {
          NormalFloat = { bg = "none" },
          FloatBorder = { bg = "none" },
          FloatTitle = { bg = "none" },
          NormalDark = { fg = t.ui.fg_dim, bg = t.ui.bg_m3 },
          MasonNormal = { bg = "none", fg = t.ui.fg_dim },
          LazyNormal = { bg = "none", fg = t.ui.fg_dim },
          TelescopeTitle = { fg = t.ui.special, bold = true },
          TelescopePromptNormal = { bg = t.ui.float.bg },
          TelescopePromptBorder = { fg = t.ui.fg, bg = t.ui.float.bg },
          TelescopeResultsNormal = { fg = t.ui.fg_dim, bg = t.ui.float.bg },
          TelescopeResultsBorder = { fg = t.ui.fg, bg = t.ui.float.bg },
          TelescopePreviewNormal = { bg = t.ui.float.bg },
          TelescopePreviewBorder = { fg = t.ui.fg, bg = t.ui.float.bg },
        }
      end,
      -- fg,fg_dim,fg_reverse,bg_dim,bg_gutter,bg_m3,bg_m2,bg_m1
      -- bg_p1,bg_p2,special,nontext,whitespace,bg_search,bg_visual
      colors = {
        theme = {
          all = { ui = { bg_gutter = "none" } },
          wave = { ui = { bg = "#0F1419" } },
        },
      },
      theme = "dragon",
    },
  },
  {
    "marko-cerovac/material.nvim",
    lazy = false,
    priority = 1010,
    init = function() vim.g.material_style = "deep ocean" end,
    opts = {
      contrast = { floating_windows = true, lsp_virtual_text = true },
      styles = {
        comments = { italic = true },
        functions = { italic = true, bold = true },
        types = { italic = true, bold = true },
      },
      plugins = {
        "flash",
        "gitsigns",
        "indent-blankline",
        "neogit",
        "noice",
        "nvim-notify",
        "nvim-cmp",
        "nvim-web-devicons",
      },
      high_visibility = { lighter = true, darker = true },
      lualine_style = "stealth",
      custom_colors = nil,
      custom_highlights = {},
    },
  },
}
