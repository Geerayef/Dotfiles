return {
  "folke/flash.nvim",
  keys = {
    { "<M-j>", mode = { "n", "x", "o" }, function() require("flash").jump() end, desc = "Flash [j]ump" },
    { "<M-t>", mode = { "n", "x", "o" }, function() require("flash").treesitter() end, desc = "Flash [T]reesitter" },
    { "<C-s>", mode = { "c" }, function() require("flash").toggle() end, desc = "Toggle Flash Search" },
    -- { "r", mode = "o", function() require("flash").remote() end, desc = "Remote Flash" },
    -- { "R", mode = { "o", "x" }, function() require("flash").treesitter_search() end, desc = "Treesitter Search" },
  },
  -- event = { "BufAdd", "CursorMoved", "CursorMovedI" },
  opts = {
    labels = "asdfghjklqwertyuiopzxcvbnm",
    label = {
      current = false,
      uppercase = true,
      after = false,
      before = true,
      exclude = "",
      style = "overlay", ---@type "eol" | "overlay" | "right_align" | "inline"
      reuse = "lowercase", ---@type "lowercase" | "all" | "none"
      distance = true,
      min_pattern_length = 0,
      rainbow = { enabled = true, shade = 5 },
    },
    search = {
      multi_window = true,
      forward = true,
      wrap = true,
      mode = "fuzzy", ---@type "exact" | "search" | "fuzzy" | fun(str) : string
      incremental = false,
      exclude = {
        "notify",
        "cmp_menu",
        "noice",
        "flash_prompt",
        function(win) return not vim.api.nvim_win_get_config(win).focusable end,
      },
      trigger = "",
      max_length = false, ---@type number | false
    },
    jump = {
      jumplist = true,
      pos = "start", ---@type "start" | "end" | "range"
      history = false,
      register = false,
      nohlsearch = false,
      autojump = false,
      inclusive = nil, ---@type boolean?
      offset = nil, ---@type number
    },
    highlight = {
      backdrop = true,
      matches = true,
      priority = 5000,
      groups = { match = "FlashMatch", current = "FlashCurrent", backdrop = "FlashBackdrop", label = "FlashLabel" },
    },
    action = nil,
    pattern = "",
    continue = false,
    config = nil,
    modes = {
      search = {
        enabled = true,
        highlight = { backdrop = false },
        jump = { history = true, register = true, nohlsearch = true },
        -- `forward` will be automatically set to the search direction
        -- `mode` is always set to `search`
        -- `incremental` is set to `true` when `incsearch` is enabled
        search = {},
      },
      char = {
        enabled = true,
        config = function(opts)
          opts.autohide = opts.autohide or (vim.fn.mode(true):find("no") and vim.v.operator == "y")
          opts.jump_labels = opts.jump_labels
            and vim.v.count == 0
            and vim.fn.reg_executing() == ""
            and vim.fn.reg_recording() == ""
          -- Show jump labels only in operator-pending mode
          -- opts.jump_labels = vim.v.count == 0 and vim.fn.mode(true):find("o")
        end,
        autohide = true,
        jump_labels = true,
        label = { exclude = "hjkliadco" },
        multi_line = true,
        -- e.g., { [";"] = "L", [","] = H }
        keys = { "f", "F", "t", "T", ";", "," },
        char_actions = function(motion)
          return { [";"] = "next", [","] = "prev", [motion:lower()] = "next", [motion:upper()] = "prev" }
        end,
        search = { wrap = false },
        highlight = { backdrop = true },
        jump = { register = false },
      },
      treesitter = {
        labels = "abcdefghijklmnopqrstuvwxyz",
        jump = { pos = "range" },
        search = { incremental = false },
        label = { before = true, after = true, style = "inline" },
        highlight = { backdrop = false, matches = false },
      },
      treesitter_search = {
        jump = { pos = "range" },
        search = { multi_window = true, wrap = true, incremental = false },
        remote_op = { restore = true },
        label = { before = true, after = true, style = "inline" },
      },
      remote = { remote_op = { restore = true, motion = true } },
    },
    prompt = {
      enabled = true,
      prefix = { { "⚡", "FlashPromptIcon" } },
      win_config = { relative = "editor", width = 1, height = 1, row = -1, col = 0, zindex = 1000 },
    },
    remote_op = { restore = false, motion = false },
  },
}
