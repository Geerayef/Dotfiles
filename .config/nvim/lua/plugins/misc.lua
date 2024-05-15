return {
  { "nvim-lua/plenary.nvim", lazy = false, priority = 1000 },
  { "nvim-tree/nvim-web-devicons", lazy = true },
  { "tpope/vim-sleuth", lazy = true, event = { "BufReadPre" } },
  { "tpope/vim-surround", lazy = true, event = { "BufEnter", "CursorMovedI" } },
  { "mfussenegger/nvim-jdtls", lazy = true, ft = "java" },
  -- { "Pocco81/true-zen.nvim", lazy = true, cmd = { "TZFocus", "TZNarrow", "TZAtaraxis", "TZMinimalist" } },
  -- { "norcalli/nvim-colorizer.lua", cmd = "ColorizerToggle", opts = { ["*"] = { RRGGBBAA = true } } },
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    opts = function()
      require("cmp").event:on("confirm_done", require("nvim-autopairs.completion.cmp").on_confirm_done())
      return { check_ts = true, ts_config = { lua = { "string" }, java = false } }
    end,
  },
  {
    "numToStr/Comment.nvim",
    event = { "BufEnter", "CursorMovedI", "BufWinEnter" },
    opts = {
      opleader = { line = "gc", block = "gb" },
      mappings = { basic = true, extra = true },
      toggler = { line = "gcc", block = "gbc" },
    },
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    event = { "BufEnter", "BufWinEnter" },
    main = "ibl",
    opts = { indent = { char = "│" } },
  },
  {
    "andymass/vim-matchup",
    event = { "BufEnter", "CursorMovedI" },
    config = function()
      vim.g.matchup_matchparen_offscreen = { method = "popup" }
      vim.g.matchup_surround_enabled = 1
      vim.g.matchup_matchparen_deferred = 1
      vim.g.matchup_override_vimtex = 1
    end,
  },
}
