local road = require("clrs.road")
local rb, rp = road.base, road.palette
return {
  { "bettervim/yugen.nvim", lazy = vim.g.theme ~= "yugen" },
  {
    "webhooked/kanso.nvim",
    lazy = vim.g.theme ~= "kanso",
    opts = { compile = true, background = { dark = "zen", light = "mist" } },
  },
  {
    "sainnhe/gruvbox-material",
    lazy = vim.g.theme ~= "gruvbox-material",
    config = function()
      vim.g.gruvbox_material_enable_italic = false
      vim.g.gruvbox_material_background = "hard"
    end,
  },
  {
    "amedoeyes/eyes.nvim",
    lazy = vim.g.theme ~= "eyes",
    opts = {
      highlights = { core = "all", plugins = "all" },
      extend = {
        highlights = {
          Normal = { bg = rb.dragonInk },
          ["@comment"] = { fg = rb.paynesGray },
          ["@function"] = { fg = rp.lotusYellow[400] },
        },
      },
    },
  },
}
