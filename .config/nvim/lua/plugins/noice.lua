local border = require("util.objects").Border
return {
  "folke/noice.nvim",
  event = "VeryLazy",
  dependencies = { "MunifTanjim/nui.nvim" },
  opts = {
    cmdline = { enabled = true, view = "cmdline" },
    messages = { enabled = true, view = "mini", view_warn = "mini", view_error = "split", view_history = "popup" },
    notify = { enabled = true, view = "mini" },
    popupmenu = { enabled = true, backend = "nui" },
    commands = { history = { view = "popup" }, last = { view = "mini" } },
    lsp = {
      progress = { enabled = false },
      hover = { enabled = false, silent = true },
      signature = { enabled = false },
      message = { enabled = true, view = "mini" },
    },
    health = { checker = false },
    presets = { bottom_search = true, command_palette = true, long_message_to_split = true },
    routes = {
      { view = "mini", filter = { event = "msg_showmode" } },
      { view = "vsplit", filter = { error = true, min_height = 10 } },
      { view = "vsplit", filter = { event = "msg_show", min_height = 10 } },
    },
    views = {
      messages = { view = "popup" },
      split = { enter = true },
      vsplit = { enter = true, size = "30%" },
      virtualtext = { format = { "   󰝤 {message}" } },
      mini = { win_options = { winblend = 90 } },
      notify = { backend = "mini" },
      popup = { border = { style = border } },
    },
  },
}
