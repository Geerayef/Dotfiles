return {
  {
    "tpope/vim-fugitive",
    event = "VeryLazy",
  },
  {
    "lewis6991/gitsigns.nvim",
    event = "VeryLazy",
    config = function()
      require("gitsigns").setup({
        signs = {
          add = { text = "+" },
          change = { text = "~" },
          delete = { text = "X" },
          topdelete = { text = "^" },
          changedelete = { text = "¬" },
          untracked = { text = "?" },
        }
      })
    end
  }
}
