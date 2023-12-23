return {
  "rebelot/heirline.nvim",
  dependencies = {
    "lewis6991/gitsigns.nvim",
    "nvim-tree/nvim-web-devicons"
  },
  event = "UiEnter",
  config = function ()
    local has_heirline, heirline = pcall(require, "heirline")
    if not has_heirline then return end
    local StatusLine = require("plugins.heirline.statusline")

    heirline.setup({
      statusline = StatusLine,
      opts = {
        disable_winbar_cb = true,
        colors = require("kanagawa.colors").setup({ theme = "dragon" })
      }
    })
  end
}
