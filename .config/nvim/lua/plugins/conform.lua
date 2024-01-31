return {
  "stevearc/conform.nvim",
  event = { "BufWritePre" },
  cmd = { "ConformInfo" },
  keys = {
    {
      "<leader>F",
      function() require("conform").format({ async = true, lsp_fallback = true }) end,
      mode = "",
      desc = "Format current buffer",
    },
  },
  opts = {
    formatters_by_ft = {
      lua = { "stylua" },
      python = { "ruff_format" },
      ocaml = { "ocamlformat" },
      fish = { "fish_indent" },
      bash = { "shfmt" },
      sh = { "shfmt" },
    },
    format_on_save = { timeout_ms = 500, lsp_fallback = true },
    formatters = {
      shfmt = { prepend_args = { "-i", "2", "-s" } },
    },
  },
}
