local kngw = require("clrs.kanagawa.palette")
return {
  {
    "bettervim/yugen.nvim",
    priority = 1010,
    lazy = function() return vim.g.theme ~= "yugen" end,
  },
  {
    "ramojus/mellifluous.nvim",
    lazy = function() return vim.g.theme ~= "mellifluous" end,
    priority = 1010,
    opts = {
      dim_inactive = false,
      colorset = "mountain",
      styles = {
        -- stylua: ignore start
        main_keywords  = { bold = false, italic = false },
        other_keywords = { bold = false, italic = false },
        types          = { bold = false, italic = false },
        functions      = { bold = true , italic = false },
        operators      = { bold = false, italic = false },
        strings        = { bold = false, italic = false },
        constants      = { bold = false, italic = false },
        comments       = { bold = false, italic = true  },
        markup = { headings = { bold = true } },
        folds = {},
        -- stylua: ignore end
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
          -- stylua: ignore start
          hl.set("@markup.list.unchecked", { bg = kngw.dragonInk1 })
          hl.set("@text.title.1.markdown"        , { link = "markdownH1" })
          hl.set("@text.title.1.marker.markdown" , { link = "markdownH1Delimiter" })
          hl.set("@text.title.2.markdown"        , { link = "markdownH2" })
          hl.set("@text.title.2.marker.markdown" , { link = "markdownH2Delimiter" })
          hl.set("@text.title.3.markdown"        , { link = "markdownH3" })
          hl.set("@text.title.3.marker.markdown" , { link = "markdownH3Delimiter" })
          hl.set("@text.title.4.markdown"        , { link = "markdownH4" })
          hl.set("@text.title.4.marker.markdown" , { link = "markdownH4Delimiter" })
          hl.set("@text.title.5.markdown"        , { link = "markdownH5" })
          hl.set("@text.title.5.marker.markdown" , { link = "markdownH5Delimiter" })
          hl.set("@text.title.6.markdown"        , { link = "markdownH6" })
          hl.set("@text.title.6.marker.markdown" , { link = "markdownH6Delimiter" })
        end,
        -- stylua: ignore end
      },
      -- stylua: ignore start
      color_overrides = { dark = { bg = function() return require("mellifluous.color").new(kngw.dragonInk1) end } },
      flat_background = { line_numbers = true, floating_windows = true, cursor_line_number = true },
      plugins = { telescope = { nvchad_like = false }, nvim_tree = { enabled = false }, neo_tree = { enabled = false }, startify = false },
      -- stylua: ignore end
    },
  },
  {
    "amedoeyes/eyes.nvim",
    lazy = function() return vim.g.theme ~= "eyes" end,
    priority = 1010,
    -- stylua: ignore start
    opts = { features = { plugins = { codeium = false, dap_ui = false, illuminate = false,
      leap = false, mason = false, mini_indentscope = false, neo_tree = false, noice = false } },
      extend = { highlights = { Normal = { bg = kngw.dragonInk1 } } }
    },
    -- stylua: ignore end
  },
  {
    "slugbyte/lackluster.nvim",
    lazy = function() return string.match(vim.g.theme, "lackluster") ~= nil end,
    priority = 1010,
    config = function()
      require("lackluster").setup({
        -- stylua: ignore start
        tweak_background = { normal = "#010101", telescope = "default" },
        tweak_highlight = {
          Normal                 = { bg   = kngw.dragonInk1, overwrite = true },
          NormalNC               = { link = "Normal"         },
          NormalFloat            = { link = "Normal"         },
          FloatTitle             = { link = "TelescopeTitle" },
          FloatBorder            = { bg   = "none"           },
          LazyNormal             = { link = "Normal"         },
          TelescopeTitle         = { bg   = "none", bold = true },
          TelescopeBorder        = { bg   = "none"           },
          TelescopePromptNormal  = { link = "Normal"         },
          TelescopeResultsNormal = { link = "Normal"         },
          TelescopePreviewNormal = { link = "Normal"         },
        },
        disable_plugins = { bufferline = true, dashboard = true, git_gutter = true, headline = true,
          indentmini = true, lightbulb = true, lsp_config = true, mason = true, mini_diff = true,
          navic = true, noice = true, rainbow_delimiter = true, scollbar = true, todo_comments = true,
          tree = true, trouble = true, which_key = true, yanky = true },
        -- stylua: ignore end
      })
    end,
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
