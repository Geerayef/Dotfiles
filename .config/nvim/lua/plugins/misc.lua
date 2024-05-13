return {
  { "nvim-lua/plenary.nvim", lazy = false, priority = 1000 },
  { "nvim-tree/nvim-web-devicons", lazy = true },
  { "tpope/vim-sleuth", lazy = true, event = { "BufReadPre" } },
  { "tpope/vim-surround", lazy = true, event = { "BufEnter", "CursorMovedI" } },
  { "Pocco81/true-zen.nvim", lazy = true, cmd = { "TZFocus", "TZNarrow", "TZAtaraxis", "TZMinimalist" } },
  { "mfussenegger/nvim-jdtls", lazy = true, ft = "java" },
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
    event = { "BufEnter", "CursorMovedI", "WinEnter", "BufWinEnter" },
    main = "ibl",
    opts = { indent = { char = "│" } },
  },
  {
    "andymass/vim-matchup",
    event = { "BufEnter", "CursorMovedI" },
    setup = function()
      vim.g.matchup_matchparen_offscreen = { method = "popup" }
      vim.g.matchup_surround_enabled = 1
      vim.g.matchup_matchparen_deferred = 1
      vim.g.matchup_override_vimtex = 1
    end,
  },
  {
    "folke/todo-comments.nvim",
    cmd = { "TodoTelescope", "TodoQuickFix", "TodoLocList" },
    opts = {
      keywords = {
        FIX = { icon = " ", color = "error", alt = { "FIXME", "BUG", "FIXIT", "ISSUE" } },
        TODO = { icon = " ", color = "info" },
        HACK = { icon = " ", color = "warning" },
        WARN = { icon = " ", color = "warning", alt = { "WARNING", "XXX" } },
        PERF = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
        NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
        TEST = { icon = "⏲ ", color = "test", alt = { "TESTING", "PASSED", "FAILED" } },
      },
    },
  },
  {
    "norcalli/nvim-colorizer.lua",
    cmd = "ColorizerToggle",
    keys = { { "<leader>ct", "<cmd>ColorizerToggle<CR>", desc = "[C]olorizer [T]oggle" } },
    opts = { ["*"] = { RGB = true, RRGGBB = true, RRGGBBAA = true, names = true, mode = "background" } },
  },
}
