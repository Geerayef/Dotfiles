return {
  {
    "williamboman/mason.nvim",
    cmd = "Mason",
    opts = {
      registries = { "github:mason-org/mason-registry" },
      ui = {
        icons = {
          package_installed = S.Icons.ui.box_check,
          package_pending = S.Icons.ui.arrow_r,
          package_uninstalled = S.Icons.ui.box_empty,
        },
        border = S.Border,
        width = 0.7,
        height = 0.5,
      },
    },
  },
  {
    "neovim/nvim-lspconfig",
    version = false,
    event = "BufReadPost",
    dependencies = { "hrsh7th/cmp-nvim-lsp" },
    opts = {
      diagnostics = require("core.diag"),
      inlay_hints = { enabled = true },
      autoformat = false,
      caps = {
        workspace = { didChangeWatchedFiles = { dynamicRegistration = true } },
        textDocument = {
          documentFormattingProvider = false,
          codelens = { enable = true },
          completion = {
            completionItem = {
              snippetSupport = true,
              resolveSupport = {
                properties = {
                  "detail",
                  "documentation",
                  "additionalTextEdits",
                },
              },
            },
          },
        },
      },
      servers = {
        ocamllsp = {
          filetypes = { "ocaml", "ocamlinterface", "opam", "dune" },
          root_patterns = { "*.ml", "*.mli", "*.opam" },
          cmd = { "ocamllsp" },
          single_file_support = true,
        },
        clangd = {
          filetypes = { "c", "h", "cpp", "chh" },
          root_patterns = { "*.clangd" },
          cmd = { "clangd" },
          single_file_support = true,
        },
        lua_ls = {
          filetypes = { "lua" },
          root_patterns = { "*.lua" },
          settings = {
            Lua = {
              completion = {
                enable = true,
                callSnippet = "Both",
                keywordSnippet = "Both",
                displayContext = 2,
              },
              diagnostics = {
                enable = true,
                globals = { "vim", "jit" },
                neededFileStatus = "Opened",
              },
              runtime = {
                version = "LuaJIT",
                path = vim.split(package.path, ";"),
              },
              workspace = {
                library = {
                  vim.env.VIMRUNTIME,
                  vim.fn.expand("$VIMRUNTIME/lua"),
                  vim.fn.expand("$VIMRUNTIME/lua/vim/lsp"),
                  vim.fn.expand("$XDG_CONFIG_HOME") .. "/nvim/lua",
                  "${3rd}/luv/library",
                },
                ignoreSubmodules = false,
                preloadFileSize = 1000,
                checkThirdParty = false,
              },
              telemetry = { enable = false },
              hint = { enable = true, setType = true },
            },
          },
          cmd = { "lua-language-server" },
          single_file_support = true,
        },
        ruff_lsp = {
          filetypes = { "python" },
          root_patterns = { "*.py" },
          cmd = { "ruff-lsp" },
          single_file_support = true,
        },
        pylsp = {
          filetypes = { "python" },
          root_patterns = { "*.py" },
          settings = {
            pylsp = {
              plugins = {
                autopep8 = { enabled = false },
                flake8 = { enabled = false },
                yapf = { enabled = false },
                mccabe = { enabled = false },
                pycodestyle = { enabled = false },
                pyflakes = { enabled = false },
                pylint = { enabled = false },
              },
            },
          },
          cmd = { "pylsp" },
          single_file_support = true,
        },
        rust_analyzer = {
          filetypes = { "rust" },
          root_patterns = { "*.rs" },
          settings = {
            ["rust-analyzer"] = {
              imports = { prefix = "self", granularity = { group = "module" } },
              cargo = { buildScripts = { enable = true } },
              procMacro = { enable = true },
            },
          },
          cmd = { "rust-analyzer" },
          single_file_support = true,
        },
        markdown_oxide = {
          filetypes = { "markdown" },
          root_patterns = { "*.md" },
          cmd = { "markdown-oxide" },
          single_file_support = true,
        },
        texlab = {
          filetypes = { "tex", "plaintex", "bib" },
          root_patterns = { "*.tex", "*.bib", ".latexmkrc" },
          settings = {
            texlab = {
              rootDirectory = nil,
              build = {
                executable = "latexmk",
                args = {
                  "-pdf",
                  "-interaction=nonstopmode",
                  "-synctex=1",
                  "%f",
                },
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
        },
        bashls = {
          filetypes = { "sh", "bash" },
          root_patterns = { ".shellcheckrc" },
          settings = {
            bashIde = { globPattern = "*@(.sh|.inc|.bash|.command)" },
          },
          cmd = { "bash-language-server", "start" },
          single_file_support = true,
        },
        biome = {
          filetypes = {
            "javascript",
            "typescript",
            "svelte",
            "json",
            "jsonc",
          },
          root_patterns = { "package.json", "biome.json", "biome.jsonc" },
          cmd = { "biome", "lsp-proxy" },
          single_file_support = true,
        },
        svelte = {
          filetypes = { "svelte", "css", "html", "javascript", "typescript" },
          root_patterns = { "biome.json", "biome.jsonc", "svelte.config.js" },
          cmd = { "pylsp" },
          single_file_support = true,
        },
        tsserver = {
          init_options = {
            preferences = { disableSuggestions = true },
            hostInfo = "neovim",
          },
          filetypes = { "javascript", "typescript" },
          root_patterns = { "package.json", "jsconfig.json", "tsconfig.json" },
          cmd = { "typescript-language-server", "--stdio" },
          single_file_support = true,
        },
      },
    },
    config = function(_, opts)
      local lspconfig = require("lspconfig")
      require("lspconfig.ui.windows").default_options.border = S.Border
      local cmp = require("cmp_nvim_lsp")
      vim.diagnostic.config(vim.deepcopy(opts.diagnostics))
      local capabilities = vim.tbl_deep_extend(
        "force",
        {},
        opts.capabilities or {},
        cmp.default_capabilities(vim.lsp.protocol.make_client_capabilities())
          or {}
      )
      local a = { on_attach = FN.LspAttach, capabilities = capabilities }
      local servers = vim.tbl_keys(opts.servers)
      local roots = S.root_markers
      local setup
      for _, s in ipairs(servers) do
        roots = vim.list_extend(roots, opts.servers[s].root_patterns)
        opts.servers[s].root_patterns = roots
        setup = vim.tbl_deep_extend("keep", a, opts.servers[s])
        lspconfig[s].setup(setup)
      end
    end,
  },
}
