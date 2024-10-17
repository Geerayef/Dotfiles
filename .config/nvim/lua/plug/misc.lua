return {
  { "nvim-lua/plenary.nvim", lazy = true },
  { "nvim-tree/nvim-web-devicons", lazy = true },
  { "mfussenegger/nvim-jdtls", ft = "java" },
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
    "lukas-reineke/indent-blankline.nvim",
    event = "BufReadPost",
    main = "ibl",
    opts = { indent = { char = "│" } },
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
  {
    "norcalli/nvim-colorizer.lua",
    cmd = "ColorizerToggle",
    opts = { ["*"] = { RRGGBBAA = true, rgb_fn = true, hsl_fn = true } },
  },
  {
    "stevearc/dressing.nvim",
    event = "VeryLazy",
    opts = {
      input = {
        title_pos = "center",
        border = S.Border,
        relative = "win",
        prefer_width = 0.4,
        max_width = { 0.8 },
        min_width = { 0.2 },
        win_options = { sidescrolloff = 2 },
        mappings = {
          n = { ["<Esc>"] = "Close", ["<C-c>"] = "Close", ["<CR>"] = "Confirm" },
          i = {
            ["<Esc>"] = "Close",
            ["<C-c>"] = "Close",
            ["<CR>"] = "Confirm",
            ["<C-p>"] = "HistoryPrev",
            ["<C-n>"] = "HistoryNext",
          },
        },
        override = function(conf)
          conf.style = "minimal"
          return conf
        end,
      },
      select = {
        backend = { "telescope", "nui", "builtin", "fzf" },
        fzf = { window = { width = 0.8, height = 0.4 } },
        nui = { border = { style = S.Border } },
        builtin = {
          border = S.Border,
          max_height = 0.6,
          mappings = {
            ["<Esc>"] = "Close",
            ["<C-c>"] = "Close",
            ["<CR>"] = "Confirm",
          },
          override = function(conf)
            conf.style = "minimal"
            return conf
          end,
        },
      },
    },
  },
}
