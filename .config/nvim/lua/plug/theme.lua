local road = require("clrs.road")
local rb, rp = road.base, road.palette
return {
  { "bettervim/yugen.nvim", lazy = vim.g.theme ~= "yugen" },
  { "vinitkumar/oscura-vim", lazy = vim.g.theme ~= "oscura" },
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
          ["@function"] = { fg = rp.lotusYellow[400] },
        },
      },
    },
  },
}
