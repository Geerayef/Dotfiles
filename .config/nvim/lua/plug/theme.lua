local kngw = require("clrs.kanagawa.palette")
return {
  {
    "ramojus/mellifluous.nvim",
    lazy = function() return vim.g.theme ~= "mellifluous" end,
    priority = 1010,
    opts = {
      dim_inactive = false,
      colorset = "mountain",
      styles = {
        main_keywords = { bold = false, italic = false },
        other_keywords = { bold = false, italic = false },
        types = { bold = false, italic = false },
        functions = { bold = true },
        operators = { bold = false, italic = false },
        strings = { bold = false, italic = false },
        constants = { bold = false, italic = false },
        comments = { italic = true },
        markup = { headings = { bold = true } },
        folds = {},
      },
      highlight_overrides = {
        dark = function(hl, c)
          hl.set("Normal", { bg = kngw.dragonInk1 })
          hl.set("NormalNC", { bg = kngw.dragonInk1 })
          hl.set("NormalFloat", { bg = kngw.dragonInk1 })
          hl.set("WinSeparator", { fg = kngw.dragonBlack5 })
          hl.set("LineNr", { bg = kngw.dragonInk1 })
          hl.set("LineNrAbove", { fg = c.comments })
          hl.set("LineNrBelow", { fg = c.comments })
          hl.set("CursorLineNr", { link = "Normal" })
          hl.set("FloatTitle", { link = "Title" })
          hl.set("TelescopeTitle", { link = "Title" })
          hl.set("LazyNormal", { link = "Normal" })
        end,
      },
      color_overrides = {
        dark = {
          bg = function()
            return require("mellifluous.color").new(kngw.dragonInk1)
          end,
        },
      },
      transparent_background = { enabled = false },
      flat_background = {
        line_numbers = true,
        floating_windows = true,
        cursor_line_number = true,
        file_tree = false,
      },
      plugins = {
        cmp = true,
        gitsigns = true,
        indent_blankline = true,
        telescope = { enabled = true, nvchad_like = true },
        nvim_tree = { enabled = false },
        neo_tree = { enabled = false },
        startify = false,
      },
    },
  },
  {
    "slugbyte/lackluster.nvim",
    lazy = function() return string.match(vim.g.theme, "lackluster") ~= nil end,
    priority = 1010,
    config = function()
      require("lackluster").setup({
        tweak_background = { normal = "#010101", telescope = "default" },
        tweak_highlight = {
          Normal = { overwrite = true, bg = "#010101" },
          NormalNC = { link = "Normal" },
          NormalFloat = { link = "Normal" },
          FloatTitle = { link = "TelescopeTitle" },
          FloatBorder = { bg = "none" },
          LazyNormal = { link = "Normal" },
          TelescopeTitle = { bg = "none", bold = true },
          TelescopeBorder = { bg = "none" },
          TelescopePromptNormal = { link = "Normal" },
          TelescopeResultsNormal = { link = "Normal" },
          TelescopePreviewNormal = { link = "Normal" },
        },
        disable_plugins = {
          telescope = true,
          bufferline = true,
          dashboard = true,
          git_gutter = true,
          headline = true,
          indentmini = true,
          lightbulb = true,
          lsp_config = true,
          mason = true,
          mini_diff = true,
          navic = true,
          noice = true,
          rainbow_delimiter = true,
          scollbar = true,
          todo_comments = true,
          tree = true,
          trouble = true,
          which_key = true,
          yanky = true,
        },
      })
    end,
  },
  {
    "rebelot/kanagawa.nvim",
    lazy = function() return vim.g.theme ~= "kanagawa" end,
    priority = 1010,
    opts = {
      compile = true,
      keywordStyle = { bold = true },
      typeStyle = { italic = false, bold = true },
      functionStyle = { italic = true, bold = false },
      overrides = function()
        return {
          Cursor = { fg = kngw.lotusYellow5 },
          WinSeparator = { fg = kngw.dragonBlack5 },
          NormalFloat = { bg = "none" },
          FloatTitle = { bg = "none", bold = true },
          FloatBorder = { bg = "none" },
          StatusLine = { bg = kngw.dragonInk1 },
          StatusLineNC = { bg = kngw.dragonInk1 },
          TelescopeTitle = { fg = kngw.dragonTeal, bg = "none", bold = true },
          TelescopeBorder = { fg = kngw.fujiWhite, bg = "none" },
        }
      end,
      colors = {
        theme = {
          all = { ui = { bg_gutter = "none" } },
          -- wave = { ui = { bg = kanagawa.dragonInk1 } },
          -- dragon = { ui = { bg = kanagawa.dragonInk1 } },
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
        Normal = { bg = kngw.dragonInk1 },
        NormalNC = { bg = kngw.dragonInk1 },
        NormalFloat = { bg = "none" },
        VertSplit = { fg = kngw.dragonBlack5, bg = kngw.dragonBlack5 },
        WinSeparator = { fg = kngw.dragonBlack5, bg = "none" },
        FloatTitle = { bg = "none", bold = true },
        FloatBorder = { bg = "none" },
        TelescopeTitle = { bg = "none", bold = true },
        TelescopeBorder = { bg = "none" },
      },
    },
  },
}
