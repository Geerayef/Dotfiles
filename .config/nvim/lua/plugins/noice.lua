local border = require("util.objects").Border
return {
  "folke/noice.nvim",
  event = "VeryLazy",
  dependencies = {
    "MunifTanjim/nui.nvim",
    { "rcarriga/nvim-notify", opts = { fps = 1, render = "compact", stages = "static" } },
  },
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
      message = { enabled = true, view = "notify" },
      hover = { enabled = false, silent = true },
      progress = { enabled = false },
      signature = { enabled = false },
      override = {
        ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
        ["vim.lsp.util.stylize_markdown"] = true,
        ["cmp.entry.get_documentation"] = true,
      },
    },
    health = { checker = false },
    presets = { bottom_search = true, command_palette = false, long_message_to_split = false },
    routes = {
      { view = "mini", filter = { event = "msg_showmode" } },
      { view = "vsplit", filter = { error = true, min_height = 10 } },
      { view = "vsplit", filter = { event = "msg_show", min_height = 10 } },
    },
    views = {
      mini = { win_options = { winblend = 100 } },
      popup = { border = { style = border } },
      notify = { backend = "notify" },
      messages = { view = "popup" },
      split = { enter = true },
      vsplit = { enter = true },
      virtualtext = { format = { "{message} 󰝤 " } },
    },
  },
}
