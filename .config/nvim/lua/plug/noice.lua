return {
  "folke/noice.nvim",
  event = "VeryLazy",
  dependencies = {
    "MunifTanjim/nui.nvim",
    {
      "rcarriga/nvim-notify",
      opts = { fps = 1, render = "minimal", stages = "static" },
    },
  },
  opts = {
    cmdline = { enabled = true, view = "cmdline" },
    messages = {
      enabled = true,
      view = "mini",
      view_warn = "mini",
      view_error = "notify",
      view_history = "messages",
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
    presets = {
      bottom_search = true,
      command_palette = true,
      long_message_to_split = false,
      inc_rename = true,
      lsp_doc_border = true,
    },
    routes = {
      { view = "split", filter = { error = true, min_height = 20 } },
      { view = "split", filter = { event = "msg_show", min_height = 20 } },
      {
        view = "mini",
        filter = {
          event = "msg_show",
          any = {
            { find = "; after #%d+" },
            { find = "; before #%d+" },
            { find = "fewer lines" },
          },
        },
      },
      {
        view = "messages",
        filter = {
          event = "msg_show",
          any = { { min_height = 5 }, { min_width = 100 } },
          ["not"] = {
            kind = {
              "confirm",
              "confirm_sub",
              "return_prompt",
              "quickfix",
              "search_count",
            },
          },
          blocking = false,
        },
        opts = { stop = false },
      },
      {
        view = "mini",
        filter = {
          event = "msg_show",
          any = { { min_height = 5 }, { min_width = 100 } },
          ["not"] = {
            kind = {
              "confirm",
              "confirm_sub",
              "return_prompt",
              "quickfix",
              "search_count",
            },
          },
          blocking = true,
        },
        opts = { stop = false },
      },
      {
        view = "mini",
        filter = { event = "msg_showmode" },
      },
    },
    views = {
      mini = { win_options = { winblend = 100 } },
      popup = {
        border = { style = S.Border },
        close = { keys = { "q", "<C-c>" } },
      },
      notify = { backend = "notify", replace = true, merge = true },
      messages = { view = "popup" },
      split = {
        enter = true,
        win_options = { wrap = false },
        close = { keys = { "q", "<C-c>" } },
      },
      vsplit = { enter = true },
      virtualtext = { format = { "    {message} 󰝤 " } },
    },
    health = { checker = false },
  },
}
