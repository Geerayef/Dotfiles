return {
  {
    "tpope/vim-fugitive",
    event = { "BufWritePost", "BufReadPre" }
  },
  {
    "lewis6991/gitsigns.nvim",
    event = "BufReadPre",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require("gitsigns").setup({
        signs = {
          add = { text = "+" },
          change = { text = "~" },
          delete = { text = "x" },
          topdelete = { text = "^" },
          changedelete = { text = "¬" },
          untracked = { text = "?" },
        }
      })
    end
  }
}
