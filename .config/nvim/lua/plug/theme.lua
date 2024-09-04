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
        "noice",
        "nvim-notify",
        "nvim-cmp",
        "nvim-web-devicons",
      },
      custom_highlights = {
        Normal = { bg = "#0A0E14" },
        NormalNC = { bg = "#0A0E14" },
        Cursor = { fg = kanagawa.lotusYellow5 },
        String = { fg = "#E9C799" },
        WinSeparator = { fg = "#393836" },
        NormalFloat = { bg = "none" },
        FloatTitle = { bg = "none", bold = true },
        FloatBorder = { bg = "none" },
        TelescopeTitle = { fg = "#949FB5", bg = "none", bold = true },
        TelescopeBorder = { fg = "#DCD7BA", bg = "none" },
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
          TelescopeTitle = { fg = p.dragonTeal, bg = "none", bold = true },
          TelescopeBorder = { fg = p.fujiWhite, bg = "none" },
        }
      end,
      colors = {
        theme = {
          all = { ui = { bg_gutter = "none" } },
          -- dragon = { ui = { bg = "#0A0E14" } },
          wave = { ui = { bg = "#0A0E14", light = "dragon" } },
        },
      },
      theme = "wave",
      background = { dark = "wave" },
    },
  },
  {
    "0xstepit/flow.nvim",
    lazy = false,
    priority = 1010,
    opts = function()
      require("flow").setup({
        transparent = true,
        fluo_color = "orange", -- pink | yellow | orange | green
        mode = "bright", -- normal | bright | desaturate | dark | dim
        aggressive_spell = true,
      })
      -- vim.cmd.highlight({ args = { "String", "guifg=#E9C799" }, bang = true })
    end,
  },
}
