return {
  "jellydn/hurl.nvim",
  ft = "hurl",
  opts = {
    debug = false,
    show_notification = false,
    mode = "split",
    formatters = { json = { "jq" } },
    mappings = {
      close = "q",
      next_panel = "<C-n>",
      prev_panel = "<C-p>",
    },
  },
}
