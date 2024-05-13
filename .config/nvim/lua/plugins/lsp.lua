local border = require("util.objects").Border
return {
  {
    "williamboman/mason.nvim",
    lazy = true,
    cmd = "Mason",
    opts = {
      registries = { "github:mason-org/mason-registry" },
      ui = {
        icons = { package_installed = "✓", package_pending = "->", package_uninstalled = "x" },
        border = border,
        width = 0.7,
        height = 0.5,
      },
    },
  },
  {
    "neovim/nvim-lspconfig",
    version = false,
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
    },
    config = function(_, opts)
      local lspconfig = require("lspconfig")
      require("lspconfig.ui.windows").default_options.border = border
      -- local mason_lspconfig = require("mason-lspconfig")
      -- mason_lspconfig.setup({ ensure_installed = vim.tbl_keys(opts.servers) })
      local has_cmplsp, cmplsp = pcall(require, "cmp_nvim_lsp")
      local lsp_attach = F.LspAttach
      vim.diagnostic.config(vim.deepcopy(opts.diagnostics))
      local capabilities = vim.tbl_deep_extend(
        "force",
        {},
        has_cmplsp and cmplsp.default_capabilities(vim.lsp.protocol.make_client_capabilities()) or {},
        opts.capabilities or {}
      )
      -- mason_lspconfig.setup_handlers({
      --   function(server_name)
      --     lspconfig[server_name].setup({
      --       on_attach = lsp_attach,
      --       capabilities = capabilities,
      --       settings = opts.servers[server_name],
      --     })
      --   end,
      -- })

      -- ~  Local LSP settings

      -- Lua
      lspconfig.lua_ls.setup({
        on_attach = lsp_attach,
        capabilities = capabilities,
        settings = {
          Lua = {
            completion = { enable = true, callSnippet = "Both", keywordSnippet = "Both", displayContext = 2 },
            diagnostics = { enable = true, globals = { "vim", "jit" }, neededFileStatus = "Opened" },
            runtime = { version = "LuaJIT", path = vim.split(package.path, ";") },
            workspace = {
              library = {
                vim.env.VIMRUNTIME,
                vim.env.MANSECT,
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
        single_file_support = true,
      })

      -- OCaml
      lspconfig.ocamllsp.setup({
        on_attach = lsp_attach,
        capabilities = capabilities,
        autostart = false,
        filetypes = { "ocaml", "menhir", "ocamlinterface", "ocamllex", "reason", "dune" },
        root_dir = lspconfig.util.root_pattern(
          "*.ml",
          "*.mli",
          "*.opam",
          "dune",
          ".ocamlformat",
          "ocamlformat",
          "package.json",
          "dune-project",
          "dune-workspace",
          ".git"
        ),
        cmd = { "ocamllsp" },
        single_file_support = true,
      })

      -- Rust
      lspconfig.rust_analyzer.setup({
        on_attach = lsp_attach,
        capabilities = capabilities,
        filetypes = { "rust" },
        root_dir = lspconfig.util.root_pattern("*.rs", "Cargo.toml", "rust-project.json", ".git"),
        settings = {
          ["rust-analyzer"] = {
            imports = { prefix = "self", granularity = { group = "module" } },
            checkOnSave = { command = "clippy" },
            cargo = { buildScripts = { enable = true } },
            procMacro = { enable = true },
          },
        },
        cmd = { "rust-analyzer" },
        single_file_support = true,
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
        cmd = { "/usr/bin/clangd" },
        single_file_support = true,
      })

      -- Markdown
      local capabilities_oxide = capabilities
      capabilities_oxide.workspace = { didChangeWatchedFiles = { dynamicRegistration = true } }
      lspconfig.markdown_oxide.setup({
        on_attach = lsp_attach,
        capabilities = capabilities_oxide,
        filetypes = { "markdown" },
        root_dir = lspconfig.util.root_pattern(".git", ".obsidian", ".moxide.toml", "*.md"),
        cmd = { "markdown-oxide" },
        single_file_support = true,
      })

      -- Python
      lspconfig.ruff_lsp.setup({
        on_attach = lsp_attach,
        capabilities = capabilities,
        filetypes = { "python" },
        root_dir = lspconfig.util.root_pattern("*.py", "__init__.py", ".git", "ruff.toml"),
        init_options = { settings = { args = {} } },
        cmd = { "ruff-lsp" },
        single_file_support = true,
      })

      -- Bash
      lspconfig.bashls.setup({
        on_attach = lsp_attach,
        capabilities = capabilities,
        filetypes = { "sh", "bash" },
        root_dir = lspconfig.util.root_pattern(".git"),
        settings = { bashIde = { globPattern = "*@(.sh|.inc|.bash|.command)" } },
        cmd = { "bash-language-server", "start" },
        single_file_support = true,
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
        cmd = { "texlab" },
        single_file_support = true,
      })

      -- YAML
      lspconfig.yamlls.setup({
        on_attach = lsp_attach,
        capabilities = capabilities,
        filetypes = { "yaml", "yaml.github", "yaml.gitlab" },
        settings = {
          yaml = {
            redhat = { telemetry = { enabled = false } },
            yamlVersion = 1.2,
            format = { enable = true, singleQuote = false, bracketSpacing = true, printWidth = 96 },
            validate = true,
            hover = true,
            completion = true,
            schemas = {},
            schemaStore = { enable = true, url = "https://www.schemastore.org/api/json/catalog.json" },
            editor = { tabSize = 2, formatOnType = true },
            disableDefaultProperties = true,
            suggest = { parentSkeletonSelectedFirst = true },
            style = { flowMapping = "forbid", flowSequence = "forbid" },
          },
        },
        cmd = { "yaml-language-server", "--stdio" },
        single_file_support = true,
      })

      lspconfig.docker_compose_language_service.setup({
        on_attach = lsp_attach,
        capabilities = capabilities,
        filetypes = { "yaml.docker-compose" },
        root_dir = lspconfig.util.root_pattern(
          "docker-compose.yaml",
          "docker-compose.yml",
          "compose.yaml",
          "compose.yml",
          ".git"
        ),
        cmd = { "docker-compose-langserver", "--stdio" },
        single_file_support = true,
      })

      -- Type/Java Script
      lspconfig.tsserver.setup({
        on_attach = lsp_attach,
        capabilities = capabilities,
        init_options = { preferences = { disableSuggestions = true }, hostInfo = "neovim" },
        filetypes = { "javascript", "javascriptreact", "javascript.jsx", "typescript", "typescriptreact", "typescript.tsx" },
        root_dir = lspconfig.util.root_pattern("package.json", "node_modules", "jsconfig.json", "tsconfig.json", ".git"),
        cmd = { "typescript-language-server", "--stdio" },
        single_file_support = true,
      })

      -- Biome
      lspconfig.biome.setup({
        on_attach = lsp_attach,
        capabilities = capabilities,
        filetypes = { "javascript", "json", "jsonc", "typescript", "typescript.tsx", "astro", "svelte", "vue" },
        root_dir = lspconfig.util.root_pattern(".git", "package.json", "biome.json", "biome.jsonc"),
        cmd = { "biome", "lsp-proxy" },
        single_file_support = false,
      })

      -- Svelte
      lspconfig.svelte.setup({
        on_attach = lsp_attach,
        capabilities = capabilities,
        filetypes = { "svelte", "css", "html", "javascript", "typescript" },
        root_dir = lspconfig.util.root_pattern("biome.json", "biome.jsonc", "svelte.config.js", ".git"),
        single_file_support = true,
      })
    end,
  },
}
