return {
  { "nvim-lua/plenary.nvim", lazy = true },
  {
    "numToStr/Comment.nvim",
    event = { "CursorHold", "CursorHoldI" },
    opts = true,
  },
  { "tpope/vim-surround", event = { "CursorHold", "CursorHoldI" } },
  { "tpope/vim-sleuth", event = { "BufNewFile", "BufReadPost" } },
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    opts = { check_ts = true, ts_config = { lua = { "string" }, java = false } },
  },
  {
    "andymass/vim-matchup",
    event = { "CursorHold", "CursorHoldI" },
    config = function()
      vim.g.matchup_mouse_enabled = 0
      vim.g.matchup_matchparen_offscreen = { method = "popup" }
      vim.g.matchup_matchparen_deferred = 1
      vim.g.matchup_surround_enabled = 1
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
  {
    "folke/flash.nvim",
    keys = {
      {
        "<M-j>",
        mode = { "n", "x", "o" },
        function() require("flash").jump() end,
        desc = "Flash [j]ump",
      },
      {
        "<M-t>",
        mode = { "n", "x", "o" },
        function() require("flash").treesitter() end,
        desc = "Flash [t]reesitter",
      },
      {
        "<C-s>",
        mode = { "c" },
        function() require("flash").toggle() end,
        desc = "Toggle Flash Search",
      },
    },
    opts = {
      labels = "asdfghjklqwertyuiopzxcvbnm",
      label = {
        current = false,
        after = false,
        before = true,
        rainbow = { enabled = true },
      },
      search = { mode = "fuzzy" },
      jump = {
        history = true,
        register = true,
        nohlsearch = true,
        autojump = true,
      },
      modes = {
        search = { enabled = true },
        char = {
          autohide = true,
          jump_labels = true,
          label = { exclude = "hjkliadcor" },
          multi_line = false,
        },
      },
      prompt = {
        prefix = { { " ⚡ ", "FlashPromptIcon" } },
        win_config = { width = 0.7, col = 1 },
      },
    },
  },
}
