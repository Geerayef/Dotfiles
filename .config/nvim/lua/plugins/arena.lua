local border = require("util.objects").Border
return {
  "dzfrias/arena.nvim",
  event = { "BufAdd" },
  opts = {
    max_items = 8,
    always_context = { "mod.rs", "init.lua", "dune" },
    ignore_current = false,
    buf_opts = {},
    per_project = false,
    window = { width = 60, height = 10, border = border, opts = {} },
    keybinds = { ["<C-c>"] = function() vim.cmd("ArenaClose") end },
    algorithm = { recency_factor = 1, frequency_factor = 0.5 },
  },
}
