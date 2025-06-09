return {
  "olimorris/codecompanion.nvim",
  opts = {
    adapters = {
      ["deepseek-r1"] = function()
        return require("codecompanion.adapters").extend("ollama", {
          name = "deepseek-r1",
          schema = {
            model = { default = "deepseek-r1:8b" },
            num_ctx = { default = 16384 },
          },
        })
      end,
    },
    strategies = {
      chat = { adapter = "deepseek-r1" },
      inline = {
        adapter = "deepseek-r1",
        keymaps = {
          accept_change = {
            modes = { n = "<leader>Sa" },
            description = "LLM [S]uggestion [a]ccept",
          },
          reject_change = {
            modes = { n = "<leader>Sr" },
            description = "LLM [S]uggestion [r]eject",
          },
        },
      },
      cmd = { adapter = "deepseek-r1" },
    },
    display = {
      action_palette = { width = 0.75, height = 0.4, provider = "fzf_lua" },
    },
  },
}
