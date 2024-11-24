return {
  { "nvim-lua/plenary.nvim", lazy = true },
  { "numToStr/Comment.nvim", event = "BufReadPost", opts = true },
  { "tpope/vim-surround", event = "BufReadPost" },
  {
    "tpope/vim-sleuth",
    event = "BufReadPost",
    config = function() vim.g.sleuth_ocaml_heuristics = 0 end,
  },
  {
    "mbbill/undotree",
    event = "BufReadPost",
    config = function()
      vim.g.undotree_WindowLayout = 4
      vim.g.undotree_ShortIndicators = 1
      vim.g.undotree_SplitWidth = 40
      vim.g.undotree_SetFocusWhenToggle = 1
      vim.g.undotree_HelpLine = 0
    end,
  },
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    opts = function()
      require("cmp").event:on(
        "confirm_done",
        require("nvim-autopairs.completion.cmp").on_confirm_done()
      )
      return {
        check_ts = true,
        ts_config = { lua = { "string" }, java = false },
      }
    end,
  },
  {
    "rachartier/tiny-inline-diagnostic.nvim",
    event = "VeryLazy",
    priority = 1000,
    opts = {
      signs = { left = "", right = "" },
      options = { show_source = true },
    },
  },
  {
    "andymass/vim-matchup",
    event = "BufReadPost",
    opts = function()
      vim.g.matchup_matchparen_offscreen = { method = "popup" }
      vim.g.matchup_surround_enabled = 1
      vim.g.matchup_matchparen_deferred = 1
      vim.g.matchup_override_vimtex = 1
    end,
  },
}
