return {
  {
    "kungfusheep/mfd.nvim",
    lazy = string.match(vim.g.theme, "mfd%-%a*") == nil,
    config = function()
      require("mfd").setup({ accessibility_contrast = 5, no_italic = false })
      vim.opt.guicursor = {
        "n:block-CursorNormal",
        "v:block-CursorVisual",
        "i:block-CursorInsert",
        "r-cr:block-CursorReplace",
        "c:block-CursorCommand",
      }
      require("mfd").enable_cursor_sync()
    end,
  },
  { "metalelf0/kintsugi-nvim", lazy = vim.g.theme ~= "kintsugi-flared" },
  { "bettervim/yugen.nvim", lazy = vim.g.theme ~= "yugen" },
}
