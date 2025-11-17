return {
  { "tarides/ocaml.nvim", ft = "ocaml", opts = { keymaps = {} } },
  { "gpanders/nvim-parinfer", ft = { "scheme", "commonlisp", "fennel" } },
  {
    "ranjithshegde/ccls.nvim",
    ft = { "c", "h", "cpp", "hpp" },
    opts = {
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
      filetypes = { "c", "cpp", "h", "hpp" },
      lsp = {
        codelens = { enabled = true, events = { "BufEnter", "BufWritePost" } },
        server = {
          name = "ccls",
          cmd = { "ccls" },
          args = { "--index=." },
          offset_encoding = "utf-32",
          root_dir = FS.root(vim.api.nvim_buf_get_name(0)),
          on_attach = F.LspAttach,
        },
      },
    },
    config = function(o) require("ccls").setup(o) end,
  },
}
