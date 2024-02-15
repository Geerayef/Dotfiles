return {
  "stevearc/conform.nvim",
  event = { "BufWritePre" },
  cmd = { "ConformInfo" },
  -- keys = {
  --   {
  --     "<leader>F",
  --     function() require("conform").format({ async = true, lsp_fallback = true }) end,
  --     mode = "n",
  --     desc = "[F]ormat current buffer",
  --   },
  -- },
  opts = {
    formatters_by_ft = {
      c = { "clang-format" },
      lua = { "stylua" },
      python = { "ruff format" },
      ocaml = { "ocamlformat" },
      fish = { "fish_indent" },
      bash = { "shfmt" },
      sh = { "shfmt" },
    },
    format_on_save = { timeout_ms = 500, lsp_fallback = true },
    formatters = { shfmt = { prepend_args = { "-i", "2", "-s" } } },
  },
}
