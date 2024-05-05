local border = require("util.objects").Border
return {
  {
    "williamboman/mason.nvim",
    lazy = true,
    cmd = "Mason",
    opts = {
      registries = { "github:mason-org/mason-registry" },
      ui = {
        icons = { package_installed = "✓", package_pending = "➜", package_uninstalled = "✗" },
        border = border,
        width = 0.7,
        height = 0.5,
      },
    },
  },
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPost", "BufNewFile" },
    dependencies = { "williamboman/mason-lspconfig.nvim", "hrsh7th/cmp-nvim-lsp" },
    opts = {
      diagnostics = require("core.diagnostics"),
      inlay_hints = { enabled = true },
      autoformat = false,
      capabilities = {
        textDocument = {
          documentFormattingProvider = false,
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
                [vim.fn.expand("$XDG_CONFIG_HOME") .. "/nvim/lua"] = true,
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
      require("lspconfig.ui.windows").default_options.border = border
      local mason_lspcfg = require("mason-lspconfig")
      local has_cmp, lspcmp = pcall(require, "cmp_nvim_lsp")
      local lsp_attach = F.LspAttach
      vim.diagnostic.config(vim.deepcopy(opts.diagnostics))
      local capabilities = vim.tbl_deep_extend(
        "force",
        {},
        has_cmp and lspcmp.default_capabilities(vim.lsp.protocol.make_client_capabilities()) or {},
        opts.capabilities or {}
      )
      mason_lspcfg.setup({ ensure_installed = vim.tbl_keys(opts.servers) })
      mason_lspcfg.setup_handlers({
        function(server_name)
          lspconfig[server_name].setup({
            on_attach = lsp_attach,
            capabilities = capabilities,
            settings = opts.servers[server_name],
          })
        end,
      })

      -- ~  Local LSP settings

      -- OCaml
      lspconfig.ocamllsp.setup({
        autostart = false,
        on_attach = lsp_attach,
        capabilities = capabilities,
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
          "dune-project",
          "dune-workspace",
          ".git"
        ),
        cmd = { "ocamllsp" },
      })

      -- Rust
      lspconfig.rust_analyzer.setup({
        on_attach = lsp_attach,
        capabilities = capabilities,
        filetypes = { "rust" },
        root_dir = lspconfig.util.root_pattern("Cargo.toml", "rust-project.json"),
        settings = {
          ["rust-analyzer"] = {
            imports = { prefix = "self", granularity = { group = "module" } },
            checkOnSave = { command = "clippy" },
            cargo = { buildScripts = { enable = true } },
            procMacro = { enable = true },
          },
        },
        cmd = { "rust-analyzer" },
      })

      -- Clangd
      lspconfig.clangd.setup({
        on_attach = lsp_attach,
        capabilities = capabilities,
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
        cmd = { "/usr/bin/clangd" },
      })

      -- Markdown
      lspconfig.marksman.setup({
        on_attach = lsp_attach,
        capabilities = capabilities,
        filetypes = { "markdown", "markdown.mdx" },
        root_dir = lspconfig.util.root_pattern(".git", ".marksman.toml"),
        single_file_support = true,
        cmd = { "marksman", "server" },
      })
      lspconfig.markdown_oxide.setup({
        on_attach = lsp_attach,
        capabilities = vim.tbl_deep_extend(
          "keep",
          {},
          capabilities,
          { workspace = { didChangeWatchedFiles = { dynamicRegistration = true } } }
        ),
        filetypes = { "markdown" },
        root_dir = lspconfig.util.root_pattern(".git", "*.md"),
        single_file_support = true,
        cmd = { "markdown-oxide" },
      })

      -- Python
      -- init_options = { settings = { args = {} } },
      lspconfig.ruff_lsp.setup({
        on_attach = lsp_attach,
        capabilities = capabilities,
        filetypes = { "python" },
        root_dir = lspconfig.util.root_pattern("*.py", "__init__.py", ".git", "ruff.toml"),
        single_file_support = true,
        cmd = { "ruff-lsp" },
      })

      -- Bash
      lspconfig.bashls.setup({
        on_attach = lsp_attach,
        capabilities = capabilities,
        filetypes = { "sh", "bash" },
        root_dir = lspconfig.util.root_pattern(".git"),
        settings = { bashIde = { globPattern = "*@(.sh|.inc|.bash|.command)" } },
        single_file_support = true,
        cmd = { "bash-language-server", "start" },
      })

      -- Texlab
      lspconfig.texlab.setup({
        on_attach = lsp_attach,
        capabilities = capabilities,
        filetypes = { "tex", "plaintex", "bib" },
        root_dir = lspconfig.util.root_pattern(".latexmkrc"),
        settings = {
          texlab = {
            rootDirectory = nil,
            build = {
              executable = "latexmk",
              args = { "-pdf", "-interaction=nonstopmode", "-synctex=1", "%f" },
              onSave = false,
              forwardSearchAfter = false,
            },
            auxDirectory = ".",
            forwardSearch = { executable = nil, args = {} },
            chktex = { onOpenAndSave = false, onEdit = false },
            diagnosticsDelay = 300,
            latexFormatter = "latexindent",
            latexindent = { ["local"] = nil, modifyLineBreaks = false },
            bibtexFormatter = "texlab",
            formatterLineLength = 96,
          },
        },
        single_file_support = true,
        cmd = { "texlab" },
      })

      -- Type/Java Script
      lspconfig.tsserver.setup({
        on_attach = lsp_attach,
        capabilities = capabilities,
        init_options = { preferences = { disableSuggestions = true }, hostInfo = "neovim" },
        root_dir = lspconfig.util.root_pattern("package.json", "node_modules", "jsconfig.json", "tsconfig.json", ".git"),
        filetypes = { "javascript", "javascriptreact", "javascript.jsx", "typescript", "typescriptreact", "typescript.tsx" },
        single_file_support = true,
        cmd = { "typescript-language-server", "--stdio" },
      })

      -- Biome
      lspconfig.biome.setup({
        on_attach = lsp_attach,
        capabilities = capabilities,
        filetypes = {
          "javascript",
          "jsx",
          "javascriptreact",
          "json",
          "jsonc",
          "typescript",
          "typescript.tsx",
          "typescriptreact",
        },
        root_dir = lspconfig.util.root_pattern("biome.json"),
        single_file_support = false,
        cmd = { "biome", "lsp-proxy" },
      })

      -- Svelte
      lspconfig.svelte.setup({
        on_attach = lsp_attach,
        capabilities = capabilities,
        root_dir = lspconfig.util.root_pattern("biome.json", "svelte.config.js", ".git"),
        filetypes = { "svelte", "css", "html", "javascript", "typescript" },
        single_file_support = true,
      })
    end,
  },
}
