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
    config = function(opts)
      require("ccls").setup(vim.tbl_deep_extend("force", opts, {
        win_config = {
          sidebar = { size = 50, position = "topleft", split = "vnew", width = 50, height = 20 },
        },
        filetypes = { "c", "cpp", "h", "hpp" },
        lsp = {
          codelens = { enabled = true, events = { "BufEnter", "BufWritePost" } },
          server = {
            name = "ccls",
            cmd = { "ccls" },
            args = { "--index=." },
            offset_encoding = "utf-32",
            root_dir = GRIM.fs.root(vim.api.nvim_buf_get_name(0)),
            on_attach = GRIM.lsp.attach,
          },
        },
      }))
    end,
  },
}
