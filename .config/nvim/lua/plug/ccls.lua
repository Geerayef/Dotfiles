return {
  "ranjithshegde/ccls.nvim",
  ft = { "c", "h", "cpp", "hpp" },
  config = {
    win_config = {
      sidebar = {
        size = 50,
        position = "topleft",
        split = "vnew",
        width = 50,
        height = 20,
      },
      float = {
        style = "minimal",
        relative = "editor",
        width = 50,
        height = 20,
        row = 0,
        col = 0,
        border = S.Border,
      },
    },
    filetypes = { "c", "cpp", "objc", "objcpp" },
    lsp = {
      codelens = { enabled = true, events = { "BufEnter", "BufWritePost" } },
    },
  },
}
