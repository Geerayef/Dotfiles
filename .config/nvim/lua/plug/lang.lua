return {
  { "tarides/ocaml.nvim", ft = "ocaml", opts = { keymaps = {} } },
  {
    "guns/vim-sexp",
    ft = { "scheme", "lisp", "commonlisp", "fennel", "clojure" },
    dependencies = { "tpope/vim-sexp-mappings-for-regular-people", "gpanders/nvim-parinfer" },
  },
  {
    "ranjithshegde/ccls.nvim",
    ft = { "c", "h", "cpp", "hpp" },
    opts = {
      win_config = {
        sidebar = { size = 50, position = "topleft", split = "vnew", width = 50, height = 20 },
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
          on_attach = LSP.attach,
        },
      },
    },
    config = function(o) require("ccls").setup(o) end,
  },
}
