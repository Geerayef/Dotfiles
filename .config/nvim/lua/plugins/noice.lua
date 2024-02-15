return {
  {
    "folke/noice.nvim",
    event = "VeryLazy",
    dependencies = { "MunifTanjim/nui.nvim" },
    opts = {
      cmdline = { enabled = true, view = "cmdline" },
      messages = { enabled = true, view = "mini" },
      notify = { enabled = true, view = "mini" },
      lsp = {
        progress = { enabled = false },
        hover = { enabled = false, silent = true },
        signature = { enabled = false },
        message = { enabled = false },
        documentation = {
          view = "hover",
          opts = {
            lang = "markdown",
            replace = true,
            render = "plain",
            format = { "{message}" },
            win_options = { concealcursor = "n", conceallevel = 3 },
          },
        },
      },
      health = { checker = false },
      presets = { bottom_search = true, command_palette = true, long_message_to_split = true },
      routes = { { view = "mini", filter = { event = "msg_showmode" } } },
      views = { virtualtext = { format = { "󰝤 {message}" } }, mini = { win_options = { winblend = 100 } } },
    },
  },
}
