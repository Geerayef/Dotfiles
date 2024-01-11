return {
  "nvim-lualine/lualine.nvim",
  dependencies = { "lewis6991/gitsigns.nvim", "nvim-tree/nvim-web-devicons" },
  event = "UiEnter",
  opts = function() return require("plugins.lualine.lualine") end,
}
