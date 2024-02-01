return {
  "neovim/nvim-lspconfig",
  event = { "BufReadPost", "BufNewFile" },
  dependencies = {
    {
      "williamboman/mason.nvim",
      lazy = true,
      cmd = "Mason",
      opts = {
        registries = { "github:mason-org/mason-registry" },
        ui = { icons = { package_installed = "✓", package_pending = "➜", package_uninstalled = "✗" } },
      },
    },
    "williamboman/mason-lspconfig.nvim",
    "hrsh7th/cmp-nvim-lsp",
  },
  opts = {
    diagnostics = require("config.diagnostics"),
    inlay_hints = { enabled = false },
    autoformat = false,
    capabilities = {
      textDocument = {
        codelens = { enable = true },
        completion = {
          completionItem = {
            snippetSupport = true,
            resolveSupport = { properties = { "documentation", "detail", "additionalTextEdits" } },
          },
        },
      },
    },
    servers = {
      lua_ls = {
        Lua = {
          completion = { enable = true, callSnippet = "Both", keywordSnippet = "Both", displayContext = 2 },
          diagnostics = { enable = true, globals = { "vim", "jit" }, neededFileStatus = "Opened" },
          runtime = { version = "LuaJIT", path = vim.split(package.path, ";") },
          workspace = {
            library = {
              vim.env.VIMRUNTIME,
              [vim.fn.expand("$VIMRUNTIME/lua")] = true,
              [vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
              [vim.fn.stdpath("config") .. "/lua"] = true,
            },
            maxPreload = 2000,
            preloadFileSize = 1000,
            checkThirdParty = true,
          },
          telemetry = { enable = false },
          hint = { enable = true, setType = true },
          root_dir = { ".stylua.toml", "stylua.toml", "*.lua", ".git", "lua/" },
        },
      },
    },
  },
  config = function(_, opts)
    local lspconfig = require("lspconfig")
    local mason_lspcfg = require("mason-lspconfig")
    local has_cmp, lspcmp = pcall(require, "cmp_nvim_lsp")
    local lsp_attach = F.LspAttach
    vim.diagnostic.config(vim.deepcopy(opts.diagnostics))
    local capabilities = vim.tbl_deep_extend(
      "force",
      {},
      vim.lsp.protocol.make_client_capabilities(),
      has_cmp and lspcmp.default_capabilities() or {},
      opts.capabilities or {}
    )
    mason_lspcfg.setup({ ensure_installed = vim.tbl_keys(opts.servers) })
    mason_lspcfg.setup_handlers({
      function(server_name)
        lspconfig[server_name].setup({
          capabilities = capabilities,
          on_attach = lsp_attach,
          settings = opts.servers[server_name],
        })
      end,
    })

    -- ~  Local LSP settings

    -- OCaml
    lspconfig.ocamllsp.setup({
      on_attach = lsp_attach,
      capabilities = capabilities,
      cmd = { "ocamllsp" },
      filetypes = { "ocaml", "ocaml.menhir", "ocaml.interface", "ocaml.ocamllex", "reason", "dune" },
      root_dir = lspconfig.util.root_pattern(
        "*.ml",
        "*.mli",
        "*.opam",
        "dune",
        ".ocamlformat",
        "ocamlformat",
        "esy.json",
        "package.json",
        ".git",
        "dune-project",
        "dune-workspace"
      ),
    })

    -- Rust
    lspconfig.rust_analyzer.setup({
      on_attach = lsp_attach,
      capabilities = capabilities,
      settings = { ["rust-analyzer"] = { checkOnSave = { command = "clippy" } } },
    })

    -- Clangd
    lspconfig.clangd.setup({
      on_attach = lsp_attach,
      capabilities = capabilities,
      cmd = { "/usr/bin/clangd" },
      filetypes = { "c", "h", "cpp", "chh", "objc", "objcpp", "cuda", "proto" },
      root_dir = lspconfig.util.root_pattern(
        ".clangd",
        ".clang-tidy",
        ".clang-format",
        "compile_commands.json",
        "compile_flags.txt",
        "configure.ac",
        ".git"
      ),
      single_file_support = true,
    })

    -- Python
    lspconfig.ruff_lsp.setup({
      on_attach = lsp_attach,
      capabilities = capabilities,
      init_options = { settings = { args = {} } },
    })

    -- Bash
    lspconfig.bashls.setup({
      cmd = { "bash-language-server", "start" },
      filetypes = { "sh", "bash" },
      settings = { bashIde = { globPattern = "*@(.sh|.inc|.bash|.command)" } },
      on_attach = lsp_attach,
      capabilities = capabilities,
      single_file_support = true,
    })
  end,
}
