return {
  { "nvim-tree/nvim-web-devicons", lazy = true },
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
      vim.notify = require "notify"
      require("notify").setup {
        fps = 1,
        render = "minimal",
        stages = "static",
        timeout = 2500,
        on_open = function(win)
          local config = vim.api.nvim_win_get_config(win)
          config.border = S.Border
          vim.api.nvim_win_set_config(win, config)
        end,
      }
    end,
  },
  {
    "norcalli/nvim-colorizer.lua",
    cmd = "ColorizerToggle",
    opts = { ["*"] = { RRGGBBAA = true, rgb_fn = true, hsl_fn = true } },
  },
  {
    "stevearc/dressing.nvim",
    event = { "WinEnter" },
    opts = {
      input = {
        title_pos = "center",
        border = S.Border,
        relative = "win",
        prefer_width = 0.4,
        max_width = { 0.8 },
        min_width = { 0.2 },
        win_options = { sidescrolloff = 2 },
        trim_prompt = false,
        mappings = {
          n = { ["<C-c>"] = "Close" },
          i = {
            ["<Esc>"] = "Close",
            ["<C-p>"] = "HistoryPrev",
            ["<C-n>"] = "HistoryNext",
          },
        },
      },
      select = {
        backend = { "telescope", "nui", "builtin" },
        fzf = { window = { width = 0.8, height = 0.4 } },
        nui = { border = { style = S.Border } },
        builtin = {
          border = S.Border,
          max_height = 0.4,
          override = function(conf)
            conf.style = "minimal"
            return conf
          end,
        },
      },
    },
  },
}
