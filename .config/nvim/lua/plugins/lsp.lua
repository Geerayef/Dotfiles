return {
  -- ~  Neovim LSP
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "hrsh7th/cmp-nvim-lsp",
    },
    opts = {
      -- ~  vim.diagnostic.config()
      diagnostics = {
        underline = true,
        virtual_text = {
          spacing = 4,
          severity = nil,
          source = "if_many",
          format = nil,
          prefix = "●",
        },
        signs = true,
        float = {
          source = "if_many",
          wrap_at = 25,
          format = function(d)
            if not d.code and not d.user_data then
              return d.message
            end

            local t = vim.deepcopy(d)
            local code = d.code
            if not code then
              if not d.user_data.lsp then
                return d.message
              end

              code = d.user_data.lsp.code
            end
            if code then
              t.message = string.format("%s [%s]", t.message, code):gsub("1. ", "")
            end
            return t.message
          end,
        },
        severity_sort = true,
        update_in_insert = false,
      },
      inlay_hints = { enabled = false },
      autoformat = false,
      -- ~  Global capabilities
      capabilites = {},
      -- ~  LSP Servers
      servers = {
        bashls = {
          bashIde = {
            globPattern = "**/*@(.sh|.inc|.bash|.command|.zsh|.zshrc|.zshenv)",
            enableSourceErrorDiagnostics = true
          }
        },
        clangd = {},
        lua_ls = {
          Lua = {
            completion = {
              keywordSnippet = "Both",
              displayContext = 3
            },
            diagnostics = {
              globals = { "vim" },
              neededFileStatus = "Opened"
            },
            runtime = {
              version = "LuaJIT"
            },
            workspace = {
              library = {
                vim.env.VIMRUNTIME,
                [vim.fn.expand("$VIMRUNTIME/lua")] = true,
                [vim.fn.stdpath("config") .. "/lua"] = true,
              },
              checkThirdParty = false
            },
            telemetry = { enable = false },
            hint = {
              enable = true,
              setType = true,
            }
          }
        },
        pylsp = {
          pylsp = {
            plugins = {
              ruff = {
                enabled = true,
                extendSelect = { "I" },
                -- config = "/home/novakovic/.config/ruff/pyproject.toml"
              },
            }
          }
        },
        rust_analyzer = {},
      },
    },
    config = function(_, opts)
      local has_lspconfig, lspconfig = pcall(require, "lspconfig")
      if not has_lspconfig then return end

      local has_mlspcfg, mason_lspconfig = pcall(require, "mason-lspconfig")
      if not has_mlspcfg then return end

      local has_cmp, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")
      if not has_cmp then return end

      local lsp_attach = function(client, bufnr)
        K.LspKeymaps(client, bufnr)
        vim.api.nvim_buf_create_user_command(bufnr, "Format", function(_)
          vim.lsp.buf.format()
        end, { desc = "Format current buffer with LSP" })
      end
      
      vim.diagnostic.config(vim.deepcopy(opts.diagnostics))

      local capabilities = vim.tbl_deep_extend(
        "force",
        {},
        vim.lsp.protocol.make_client_capabilities(),
        has_cmp and cmp_nvim_lsp.default_capabilities() or {},
        opts.capabilities or {}
      )

      mason_lspconfig.setup {
        ensure_installed = vim.tbl_keys(opts.servers),
      }

      mason_lspconfig.setup_handlers({
        function(server_name)
          lspconfig[server_name].setup {
            capabilities = capabilities,
            on_attach = lsp_attach,
            settings = opts.servers[server_name],
          }
        end,
      })

    end
  },
  -- ~ Mason
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
  }
}
