local kp = require("clrs.kanagawa.palette")
return {
  {
    "vinitkumar/oscura-vim",
    priority = 1010,
    lazy = vim.g.theme ~= "oscura",
  },
  {
    "bettervim/yugen.nvim",
    priority = 1010,
    lazy = vim.g.theme ~= "yugen",
  },
  {
    "amedoeyes/eyes.nvim",
    priority = 1010,
    lazy = vim.g.theme ~= "eyes",
    opts = {
      extend = { highlights = { Normal = { bg = kp.dragonInk1 } } },
      features = {
        plugins = {
          codeium = false,
          dap_ui = false,
          illuminate = false,
          leap = false,
          mason = false,
          mini_indentscope = false,
          neo_tree = false,
          noice = false,
        },
      },
    },
  },
}
