return {
  "stevearc/dressing.nvim",
  event = "VeryLazy",
  opts = {
    input = {
      title_pos = "center",
      border = S.Border,
      relative = "win",
      prefer_width = 0.4,
      max_width = { 0.8 },
      min_width = { 0.4 },
      buf_options = {},
      win_options = {
        wrap = false,
        list = true,
        listchars = "precedes:…,extends:…",
        sidescrolloff = 2,
      },
      mappings = {
        n = {
          ["<Esc>"] = "Close",
          ["<C-c>"] = "Close",
          ["<CR>"] = "Confirm",
        },
        i = {
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
      get_config = nil,
    },
    select = {
      backend = { "telescope", "nui", "builtin", "fzf" },
      -- telescope = require('telescope.themes').get_ivy({...})
      fzf = { window = { width = 0.8, height = 0.4 } },
      nui = {
        position = "50%",
        size = nil,
        relative = "editor",
        border = { style = S.Border },
        buf_options = { swapfile = false, filetype = "DressingSelect" },
        win_options = { winblend = 0 },
        max_width = 80,
        max_height = 40,
        min_width = 40,
        min_height = 10,
      },
      builtin = {
        show_numbers = true,
        border = S.Border,
        relative = "editor",
        buf_options = {},
        win_options = { cursorline = true, cursorlineopt = "both" },
        width = nil,
        max_width = { 0.8 },
        min_width = { 0.4 },
        height = nil,
        max_height = 0.6,
        min_height = { 0.2 },
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
      get_config = nil,
    },
  },
}
