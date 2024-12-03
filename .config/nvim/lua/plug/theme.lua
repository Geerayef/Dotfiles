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
  -- stylua: ignore end
}
