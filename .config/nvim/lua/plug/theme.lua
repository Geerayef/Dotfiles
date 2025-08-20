local rb = require("clrs.road").base
return {
  { "bettervim/yugen.nvim", priority = 1010, lazy = vim.g.theme ~= "yugen" },
  { "vinitkumar/oscura-vim", priority = 1010, lazy = vim.g.theme ~= "oscura" },
  {
    "amedoeyes/eyes.nvim",
    priority = 1010,
    lazy = vim.g.theme ~= "eyes",
    opts = {
      extend = { highlights = { Normal = { bg = rb.dragonInk } } },
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
