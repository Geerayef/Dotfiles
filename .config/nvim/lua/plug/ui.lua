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
