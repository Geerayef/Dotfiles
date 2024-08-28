return {
  {
    "marko-cerovac/material.nvim",
    cond = function() return vim.g.theme == "material" end,
    lazy = false,
    priority = 1010,
    init = function() vim.g.material_style = "deep ocean" end,
    opts = {
      contrast = { lsp_virtual_text = true },
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
      high_visibility = { darker = true },
      custom_highlights = {
        Normal = { bg = "#0A0E14" },
        NormalNC = { bg = "#0A0E14" },
        Cursor = { fg = "#FFF779" },
        NormalFloat = { bg = "none" },
        FloatTitle = { bold = true },
        TelescopeBorder = { fg = "#C5C9C5", bg = "none" },
      },
    },
  },
  {
    "rebelot/kanagawa.nvim",
    cond = function() return vim.g.theme == "kanagawa" end,
    lazy = false,
    priority = 1010,
    opts = {
      compile = true,
      functionStyle = { italic = true, bold = true },
      typeStyle = { italic = true, bold = true },
      keywordStyle = { bold = true },
      statementStyle = {},
      overrides = function(colors)
        local p = colors.palette
        return {
          Cursor = { fg = "#FFF779" },
          NormalFloat = { bg = "none" },
          FloatTitle = { bg = "none", bold = true },
          FloatBorder = { bg = "none" },
          TelescopeBorder = { fg = p.dragonWhite, bg = "none" },
          WinSeparator = { fg = p.dragonBlack5 },
          TelescopeTitle = { fg = p.dragonTeal, bg = "none", bold = true },
        }
      end,
      colors = {
        theme = {
          all = { ui = { bg_gutter = "none" } },
          dragon = { ui = { bg = "#0A0E14" } },
        },
      },
      theme = "dragon",
      background = { dark = "dragon", light = "wave" },
    },
  },
}
