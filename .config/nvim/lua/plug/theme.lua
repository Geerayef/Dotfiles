local kanagawa = require("clrs.kanagawa.palette")
return {
  {
    "marko-cerovac/material.nvim",
    lazy = false,
    priority = 1010,
    init = function() vim.g.material_style = "deep ocean" end,
    opts = {
      contrast = { lsp_virtual_text = true },
      high_visibility = { darker = false },
      lualine_style = "stealth",
      styles = {
        comments = { italic = true },
        strings = { bold = true },
        keywords = { bold = true },
        functions = { italic = true, bold = false },
        variables = { italic = false },
        operators = {},
        types = { italic = false, bold = true },
      },
      plugins = {
        "flash",
        "gitsigns",
        "indent-blankline",
        "neogit",
        "nvim-cmp",
        "nvim-notify",
        "nvim-web-devicons",
      },
      custom_highlights = {
        Normal = { bg = kanagawa.dragonInk },
        NormalNC = { bg = kanagawa.dragonInk },
        NormalFloat = { bg = "none" },
        FloatTitle = { bg = "none", bold = true },
        FloatBorder = { bg = "none" },
        Cursor = { fg = kanagawa.lotusYellow5 },
        String = { fg = kanagawa.lotusYellow4 },
        Operator = { fg = "#FFFFFF" },
        WinSeparator = { fg = kanagawa.dragonBlack5 },
        TelescopeTitle = { fg = kanagawa.dragonTeal, bg = "none", bold = true },
        TelescopeBorder = { fg = kanagawa.fujiWhite, bg = "none" },
      },
    },
  },
  {
    "rebelot/kanagawa.nvim",
    lazy = false,
    priority = 1010,
    opts = {
      compile = true,
      keywordStyle = { bold = true },
      typeStyle = { italic = false, bold = true },
      functionStyle = { italic = true, bold = false },
      -- statementStyle = {},
      overrides = function(colors)
        local p = colors.palette
        return {
          Cursor = { fg = kanagawa.lotusYellow5 },
          NormalFloat = { bg = "none" },
          WinSeparator = { fg = p.dragonBlack5 },
          FloatTitle = { bg = "none", bold = true },
          FloatBorder = { bg = "none" },
          StatusLine = { bg = kanagawa.dragonInk },
          StatusLineNC = { bg = kanagawa.dragonInk },
          TelescopeTitle = { fg = p.dragonTeal, bg = "none", bold = true },
          TelescopeBorder = { fg = p.fujiWhite, bg = "none" },
        }
      end,
      colors = {
        theme = {
          all = { ui = { bg_gutter = "none" } },
          wave = { ui = { bg = kanagawa.dragonInk, light = "dragon" } },
        },
      },
      theme = "wave",
      background = { dark = "wave" },
    },
  },
}
