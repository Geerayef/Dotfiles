return {
  { "nvim-lua/plenary.nvim", lazy = true },
  {
    "numToStr/Comment.nvim",
    event = { "CursorHold", "CursorHoldI" },
    opts = true,
  },
  { "tpope/vim-surround", event = { "CursorHold", "CursorHoldI" } },
  {
    "tpope/vim-sleuth",
    event = { "BufNewFile", "BufReadPost" },
    config = function() vim.g.sleuth_ocaml_heuristics = 0 end,
  },
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    opts = function()
      -- require("cmp").event:on(
      --   "confirm_done",
      --   require("nvim-autopairs.completion.cmp").on_confirm_done()
      -- )
      return {
        check_ts = true,
        ts_config = { lua = { "string" }, java = false },
      }
    end,
  },
  {
    "andymass/vim-matchup",
    event = { "CursorHold", "CursorHoldI" },
    opts = function()
      vim.g.matchup_matchparen_offscreen = { method = "popup" }
      vim.g.matchup_surround_enabled = 1
      vim.g.matchup_matchparen_deferred = 1
      vim.g.matchup_override_vimtex = 1
    end,
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
}
