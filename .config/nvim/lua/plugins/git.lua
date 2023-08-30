return {
  {
    "tpope/vim-fugitive",
  },
  {
    "lewis6991/gitsigns.nvim",
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
