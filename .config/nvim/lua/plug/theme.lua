local kngw = require("clrs.kanagawa.palette")
return {
  {
    "bettervim/yugen.nvim",
    priority = 1010,
    lazy = function() return vim.g.theme ~= "yugen" end,
  },
  -- stylua: ignore start
  {
    "amedoeyes/eyes.nvim",
    priority = 1010,
    lazy = function() return vim.g.theme ~= "eyes" end,
    opts = { features = { plugins = { codeium = false, dap_ui = false, illuminate = false,
      leap = false, mason = false, mini_indentscope = false, neo_tree = false, noice = false } },
      extend = { highlights = { Normal = { bg = kngw.dragonInk1 } } }
    },
  },
  {
    "ramojus/mellifluous.nvim",
    priority = 1010,
    lazy = function() return vim.g.theme ~= "mellifluous" end,
    opts = {
      dim_inactive = false,
      colorset = "mountain",
      styles = {
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
      },
      color_overrides = { dark = { bg = function() return require("mellifluous.color").new(kngw.dragonInk1) end } },
      flat_background = { line_numbers = true, floating_windows = true, cursor_line_number = true },
      plugins = { telescope = { nvchad_like = false }, nvim_tree = { enabled = false }, neo_tree = { enabled = false }, startify = false },
    },
  },
  -- stylua: ignore end
}
