return {
  { "nvim-lua/plenary.nvim", lazy = true },
  { "nvim-tree/nvim-web-devicons", lazy = true },
  { "MunifTanjim/nui.nvim", lazy = true },
  { "tpope/vim-sleuth", event = "BufEnter" },
  { "tpope/vim-surround", event = "BufReadPost" },
  { "numToStr/Comment.nvim", event = "BufReadPost", opts = true },
  {
    "rcarriga/nvim-notify",
    event = "VeryLazy",
    opts = function()
      vim.notify = require("notify")
      require("notify").setup({
        fps = 1,
        render = "minimal",
        stages = "static",
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
    opts = { ["*"] = { RRGGBBAA = true } },
  },
  {
    "mfussenegger/nvim-jdtls",
    ft = "java",
    cond = function() return vim.bo.ft == "java" end,
  },
  -- {
  --   "williamboman/mason.nvim",
  --   cmd = "Mason",
  --   opts = {
  --     registries = { "github:mason-org/mason-registry" },
  --     ui = {
  --       icons = {
  --         package_installed = S.Icons.ui.box_check,
  --         package_pending = S.Icons.ui.arrow_r,
  --         package_uninstalled = S.Icons.ui.box_empty,
  --       },
  --       border = S.Border,
  --       width = 0.7,
  --       height = 0.5,
  --     },
  --   },
  -- },
}
