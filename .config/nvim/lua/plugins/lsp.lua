return {
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "hrsh7th/cmp-nvim-lsp",
    },
    opts = {
      diagnostics = require("config.diagnostics"),
      inlay_hints = { enabled = false },
      autoformat = false,
      capabilities = {},
      servers = {
        lua_ls = {
          Lua = {
            completion = { keywordSnippet = "Both", displayContext = 3 },
            diagnostics = { globals = { "vim", "jit" }, neededFileStatus = "Opened" },
            runtime = { version = "LuaJIT" },
            workspace = {
              library = {
                vim.env.VIMRUNTIME,
                [vim.fn.expand("$VIMRUNTIME/lua")] = true,
                [vim.fn.stdpath("config") .. "/lua"] = true,
              },
              checkThirdParty = false
            },
            telemetry = { enable = false },
            hint = { enable = true, setType = true }
          }
        },
      },
    },
    config = function(_, opts)
      local has_lspconfig, lspconfig = pcall(require, "lspconfig")
      if not has_lspconfig then return end

      local has_mlspcfg, mason_lspconfig = pcall(require, "mason-lspconfig")
      if not has_mlspcfg then return end

      local has_cmp, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")
      if not has_cmp then return end

      vim.diagnostic.config(vim.deepcopy(opts.diagnostics))

      local lsp_attach = function(client, bufnr)
        K.LspKeymaps(client, bufnr)
        -- Format
        vim.api.nvim_buf_create_user_command(bufnr, "FormatLSP", function(_)
          vim.lsp.buf.format()
        end, { desc = "Format current buffer with LSP" })
        -- Code lens
        if client.resolved_capabilities.code_lens then
          local codelens = vim.api.nvim_create_augroup("LSPCodeLens", { clear = true })
          vim.api.nvim_create_autocmd({ "BufEnter", "InsertLeave", "CursorHold" }, {
            group = codelens,
            callback = function() vim.lsp.codelens.refresh() end,
            buffer = bufnr,
          })
        end
      end

      local capabilities = vim.tbl_deep_extend(
        "force",
        {},
        vim.lsp.protocol.make_client_capabilities(),
        has_cmp and cmp_nvim_lsp.default_capabilities() or {},
        opts.capabilities or {}
      )

      capabilities.textDocument.completion.completionItem.snippetSupport = true
      capabilities.textDocument.completion.completionItem.resolveSupport = {
        properties = {
          "documentation",
          "detail",
          "additionalTextEdits"
        }
      }

      mason_lspconfig.setup({ ensure_installed = vim.tbl_keys(opts.servers) })

      mason_lspconfig.setup_handlers({ function(server_name)
        lspconfig[server_name].setup({
          capabilities = capabilities,
          on_attach = lsp_attach,
          settings = opts.servers[server_name],
        })
      end })

      -- ~  Local LSP settings

      -- OCaml
      lspconfig.ocamllsp.setup({
        cmd = { "ocamllsp" },
        filetypes = { "ocaml", "ocaml.menhir", "ocaml.interface", "ocaml.ocamllex", "reason", "dune" },
        root_dir = lspconfig.util.root_pattern(
          "*.ml"
          , "*.mli"
          , "*.opam"
          , "ocamlformat"
          , "esy.json"
          , "package.json"
          , ".git"
          , "dune-project"
          , "dune-workspace"
        ),
        on_attach = lsp_attach,
        capabilities = capabilities
      })

      -- Rust
      -- lspconfig.rust_analyzer.setup({
      --   on_attach = lsp_attach,
      --   capabilities = capabilities,
      --   settings = {
      --     ["rust-analyzer"] = {
      --       checkOnSave = {
      --         command = "clippy",
      --       },
      --     },
      --   },
      -- })

      -- Clangd
      lspconfig.clangd.setup({
        cmd = { "/usr/bin/clangd" },
        filetypes = { "c", "cpp", "objc", "objcpp", "cuda", "proto" },
        root_dir = lspconfig.util.root_pattern(
          '.clangd'
          , '.clang-tidy'
          , '.clang-format'
          , 'compile_commands.json'
          , 'compile_flags.txt'
          , 'configure.ac'
          , '.git'
        ),
        on_attach = lsp_attach,
        capabilities = capabilities,
        single_file_support = true,
      })

      -- Python
      lspconfig.ruff_lsp.setup({
        on_attach = lsp_attach,
        init_options = {
          settings = {
            -- Ruff cli args
            args = {}
          }
        }
      })
      -- lspconfig.pylsp.setup({
      --   settings = {
      --     pylsp = {
      --       configurationSources = { 'flake8' } ,
      --       plugins = {
      --         ruff = {
      --           enabled = true,
      --           extendSelect = { "I" },
      --           lineLength = 128,
      --           config = "/home/novakovic/.config/ruff/pyproject.toml"
      --         },
      --         flake8 = {
      --           enabled = true,
      --         },
      --         pycodestyle = {
      --           enabled = false,
      --         },
      --         mccabe = {
      --           enabled = false,
      --         },
      --         pyflakes = {
      --           enabled = false,
      --         },
      --       }
      --     }
      --   }
      -- })

      -- Bash
      lspconfig.bashls.setup({
        cmd = { "bash-language-server", "start" },
        filetypes = { "sh", "bash" },
        settings = { bashIde = { globPattern = "*@(.sh|.inc|.bash|.command)" } },
        on_attach = lsp_attach,
        capabilities = capabilities,
        single_file_support = true,
      })
    end
  },
  {
    "williamboman/mason.nvim",
    cmd = "Mason",
    opts = {
      ui = {
        icons = {
          package_installed = "✓",
          package_pending = "➜",
          package_uninstalled = "✗"
        }
      },
    },
  },
  -- {
  --   "simrat39/rust-tools.nvim",
  --   event = { "VeryLazy", "BufReadPost" },
  --   config = function()
  --     local rt = require("rust-tools")
  --
  --     rt.setup({
  --       server = {
  --         on_attach = function(client, bufnr)
  --           K.LspKeymaps(client, bufnr)
  --           vim.api.nvim_buf_create_user_command(bufnr, "FormatLSP", function(_)
  --             vim.lsp.buf.format()
  --           end, { desc = "Format current buffer with LSP" })
  --         end
  --       },
  --     })
  --   end
  -- },
}
