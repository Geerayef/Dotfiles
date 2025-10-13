local rb = require("clrs.road").base
return {
  { "bettervim/yugen.nvim", lazy = vim.g.theme ~= "yugen" },
  { "vinitkumar/oscura-vim", lazy = vim.g.theme ~= "oscura" },
  {
    "amedoeyes/eyes.nvim",
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
