return {
  "dzfrias/arena.nvim",
  opts = {
    max_items = 8,
    always_context = { "mod.rs", "init.lua" },
    ignore_current = false,
    buf_opts = {},
    per_project = false,
    window = {
      width = 60,
      height = 10,
      border = "rounded",
      opts = {},
    },
    -- keybinds = { ["e"] = function() vim.cmd("echo \"Hello from the arena!\"") end },
    algorithm = { recency_factor = 1, frequency_factor = 0.5 },
  },
}
