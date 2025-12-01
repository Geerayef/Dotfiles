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
