local border = require("util.objects").Border
return {
  "folke/noice.nvim",
  event = "VeryLazy",
  dependencies = { "rcarriga/nvim-notify", "MunifTanjim/nui.nvim" },
  opts = {
    cmdline = { enabled = true, view = "cmdline" },
    messages = {
      enabled = true,
      view = "mini",
      view_warn = "mini",
      view_error = "notify",
      view_history = "popup",
      view_search = "virtualtext",
    },
    notify = { enabled = true, view = "notify" },
    popupmenu = { enabled = true, backend = "nui" },
    commands = { history = { view = "popup" }, last = { view = "mini" } },
    lsp = {
      hover = { enabled = false, silent = true },
      progress = { enabled = false },
      message = { enabled = true, view = "notify" },
      signature = { enabled = true },
    },
    health = { checker = false },
    presets = { bottom_search = true, command_palette = true, long_message_to_split = true },
    routes = {
      { view = "mini", filter = { event = "msg_showmode" } },
      { view = "vsplit", filter = { error = true, min_height = 10 } },
      { view = "vsplit", filter = { event = "msg_show", min_height = 10 } },
    },
    views = {
      mini = { border = { style = "single" }, win_options = { winblend = 100 } },
      popup = { border = { style = border } },
      notify = { backend = "notify" },
      messages = { view = "popup" },
      split = { enter = true },
      vsplit = { enter = true },
      virtualtext = { format = { "{message} 󰝤 " } },
    },
  },
}
