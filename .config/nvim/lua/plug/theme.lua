local kngw = require("clrs.kanagawa.palette")
return {
  {
    "rebelot/kanagawa.nvim",
    lazy = function() return vim.g.theme ~= "kanagawa" end,
    priority = 1010,
    opts = {
      compile = true,
      keywordStyle = { bold = true },
      typeStyle = { italic = false, bold = true },
      functionStyle = { italic = true, bold = false },
      -- statementStyle = {},
      overrides = function()
        return {
          Cursor = { fg = kngw.lotusYellow5 },
          WinSeparator = { fg = kngw.dragonBlack5 },
          NormalFloat = { bg = "none" },
          FloatTitle = { bg = "none", bold = true },
          FloatBorder = { bg = "none" },
          StatusLine = { bg = kngw.dragonInk },
          StatusLineNC = { bg = kngw.dragonInk },
          TelescopeTitle = {
            fg = kngw.dragonTeal,
            bg = "none",
            bold = true,
          },
          TelescopeBorder = { fg = kngw.fujiWhite, bg = "none" },
        }
      end,
      colors = {
        theme = {
          all = { ui = { bg_gutter = "none" } },
          -- wave = { ui = { bg = kanagawa.dragonInk } },
          -- dragon = { ui = { bg = kanagawa.dragonInk } },
        },
      },
      theme = "dragon",
      background = { dark = "dragon" },
    },
  },
  {
    "rose-pine/neovim",
    name = "rose-pine",
    lazy = function() return vim.g.theme ~= "rose-pine" end,
    priority = 1010,
    opts = {
      variant = "main",
      dark_variant = "main",
      enable = { legacy_highlights = false },
      highlight_groups = {
        Normal = { bg = kngw.dragonInk },
        NormalNC = { bg = kngw.dragonInk },
        NormalFloat = { bg = "none" },
        VertSplit = { fg = "muted", bg = "muted" },
        WinSeparator = { fg = "muted", bg = "none" },
        FloatTitle = { bg = "none", bold = true },
        FloatBorder = { bg = "none" },
        TelescopeTitle = { bg = "none", bold = true },
        TelescopeBorder = { bg = "none" },
      },
    },
  },
}
