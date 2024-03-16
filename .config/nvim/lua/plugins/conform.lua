return {
  "stevearc/conform.nvim",
  event = { "BufWritePre" },
  cmd = { "ConformInfo" },
  keys = {
    {
      "<leader>F",
      function() require("conform").format({ async = true, lsp_fallback = true }) end,
      mode = "n",
      desc = "[F]ormat current buffer",
    },
  },
  opts = {
    formatters_by_ft = {
      c = { "clang-format" },
      lua = { "stylua" },
      python = { "ruff_fix", "ruff_format" },
      ocaml = { "ocamlformat" },
      fish = { "fish_indent" },
      bash = { "beautysh" },
      sh = { "beautysh" },
      css = { "prettier" },
      scss = { "prettier" },
      yaml = { "prettier" },
      markdown = { "prettier" },
      html = { "prettier" },
      json = { "biome" },
      jsonc = { "biome" },
      javascript = { "biome" },
      typescript = { "biome" },
    },
    format_on_save = { timeout_ms = 500, lsp_fallback = true },
    formatters = {
      beautysh = { prepend_args = { "-i", "2" } },
      biome = { command = "biome", stdin = false, inherit = false, args = { "format", "$FILENAME", "--write" } },
      prettierd = { command = "prettier", stdin = true, args = { "--stdin-filepath", "$FILENAME" } },
    },
  },
}
