return {
  "stevearc/conform.nvim",
  cmd = "ConformInfo",
  keys = {
    {
      "<leader>F",
      function() require("conform").format({ async = true, lsp_fallback = true }) end,
      mode = "n",
      desc = "[F]ormat current buffer",
    },
  },
  config = function()
    vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
    require("conform").setup({
      -- format_after_save = { async = false, lsp_fallback = true },
      formatters_by_ft = {
        c = { "clang-format" },
        cpp = { "clang-format" },
        ocaml = { "ocamlformat" },
        dune = { "dune" },
        haskell = { "fourmolu" },
        cabal = { "cabal-gild" },
        rust = { "rustfmt" },
        go = { "gofumpt" },
        lua = { "stylua" },
        python = { "ruff_format" },
        fish = { "fish_indent" },
        sh = { "shfmt" },
        bash = { "shfmt" },
        hurl = { "hurlfmt" },
        markdown = { "mdformat" },
        json = { "biome" },
        jsonc = { "biome" },
        javascript = { "biome" },
        typescript = { "biome" },
        yaml = { "yamlfmt" },
      },
      formatters = {
        ["clang-format"] = { prepend_args = { "--style=file" } },
        dune = { command = "dune", args = { "format-dune-file" } },
        ruff_format = {
          command = "ruff",
          args = {
            "format",
            "--force-exclude",
            "--stdin-filename",
            "$FILENAME",
            "-",
          },
          stdin = true,
        },
        beautysh = { prepend_args = { "-i", "2" } },
        shfmt = {
          inherit = false,
          command = "shfmt",
          args = { "-i", "2", "-s", "-bn", "-ci", "--filename", "$FILENAME" },
        },
        ["cabal-gild"] = {
          command = "cabal-gild",
          args = { "--crlf=strict", "--stdin", "$FILENAME" },
          stdin = true,
        },
        gofumpt = {
          prepend_args = { "-extra" },
        },
      },
    })
  end,
}
