return {
  "jellydn/hurl.nvim",
  ft = "hurl",
  cond = vim.g.vscode == nil,
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
