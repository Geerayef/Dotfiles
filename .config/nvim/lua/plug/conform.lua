return {
  "stevearc/conform.nvim",
  event = "BufWritePre",
  cmd = "ConformInfo",
  keys = {
    {
      "<leader>F",
      function()
        require("conform").format({ async = true, lsp_fallback = true })
      end,
      mode = "n",
      desc = "[F]ormat current buffer",
    },
  },
  opts = {
    formatters_by_ft = {
      c = { "clang-format" },
      cpp = { "clang-format" },
      ocaml = { "ocamlformat" },
      rust = { "rustfmt" },
      go = { "gofmt" },
      lua = { "stylua" },
      python = { "ruff_format" },
      fish = { "fish_indent" },
      sh = { "shfmt" },
      bash = { "shfmt" },
      yaml = { "prettier" },
      css = { "prettier" },
      scss = { "prettier" },
      html = { "prettier" },
      json = { "biome" },
      jsonc = { "biome" },
      javascript = { "biome" },
      typescript = { "biome" },
    },
    format_on_save = { timeout_ms = 500, lsp_format = "fallback" },
    formatters = {
      beautysh = { prepend_args = { "-i", "2" } },
      shfmt = {
        inherit = false,
        command = "shfmt",
        args = {
          "--indent",
          "2",
          "--simplify",
          "--binary-next-line",
          "--case-indent",
          "-filename",
          "$FILENAME",
        },
      },
      biome = {
        inherit = false,
        command = "biome",
        stdin = false,
        args = function()
          return {
            "format",
            "--config-path=" .. vim.fn.expand("$XDG_CONFIG_HOME") .. "/biome",
            "--write",
            "$FILENAME",
          }
        end,
      },
      prettier = { command = "prettier", stdin = true, args = { "$FILENAME" } },
      ruff_format = {
        command = "ruff",
        args = {
          "format",
          "--config=$XDG_CONFIG_HOME/ruff/ruff.toml",
          "--force-exclude",
          "--stdin-filename",
          "$FILENAME",
          "-",
        },
        stdin = true,
      },
    },
  },
}
