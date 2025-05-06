return {
  -- {
  --   name = "porphyrio",
  --   dir = "~/dev/porphyrio/",
  --   lazy = false,
  --   priority = 1020,
  -- },
  { "nvim-lua/plenary.nvim", lazy = true },
  { "nvim-tree/nvim-web-devicons", lazy = true },
  { "tpope/vim-surround", event = { "CursorHold", "CursorHoldI" } },
  { "tpope/vim-sleuth", event = { "BufNewFile", "BufReadPost" } },
  {
    "mistweaverco/kulala.nvim",
    ft = { "http", "rest" },
    opts = { global_keymaps = false },
  },
  {
    "numToStr/Comment.nvim",
    event = { "CursorHold", "CursorHoldI" },
    opts = true,
  },
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    opts = { check_ts = true, ts_config = { lua = { "string" }, java = false } },
  },
  {
    "andymass/vim-matchup",
    lazy = true,
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
    lazy = true,
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
      prompt = { prefix = { { " ⚡ ", "FlashPromptIcon" } } },
    },
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    event = "BufReadPost",
    main = "ibl",
    opts = { indent = { char = "│" } },
  },
  {
    "rcarriga/nvim-notify",
    event = "VeryLazy",
    opts = function()
      vim.notify = require("notify")
      require("notify").setup({
        fps = 1,
        render = "minimal",
        stages = "static",
        timeout = 2500,
        on_open = function(win)
          local config = vim.api.nvim_win_get_config(win)
          config.border = S.Border
          vim.api.nvim_win_set_config(win, config)
        end,
      })
    end,
  },
  {
    "norcalli/nvim-colorizer.lua",
    cmd = "ColorizerToggle",
    opts = { ["*"] = { RRGGBBAA = true, rgb_fn = true, hsl_fn = true } },
  },
}
