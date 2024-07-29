return {
  {
    "hrsh7th/nvim-cmp",
    version = false,
    event = { "InsertEnter", "CmdlineEnter" },
    dependencies = {
      {
        "L3MON4D3/LuaSnip",
        build = "make install_jsregexp",
        opts = { delete_check_events = "TextChanged" },
      },
      {
        "rafamadriz/friendly-snippets",
        config = function() require("luasnip.loaders.from_vscode").lazy_load() end,
      },
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-nvim-lsp-signature-help",
      "hrsh7th/cmp-nvim-lua",
      "hrsh7th/cmp-cmdline",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "onsails/lspkind.nvim",
      "saadparwaiz1/cmp_luasnip",
    },
    -- stylua: ignore start
    opts = function()
      local cmp = require("cmp")
      local ls = require("luasnip")
      local lspkind = require("lspkind")
      local devicon = require("nvim-web-devicons")
      local t = function(str) return vim.api.nvim_replace_termcodes(str, true, true, true) end
      cmp.setup.cmdline({ "/", "?" }, {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({ { name = "buffer", keyword_length = 2, max_item_count = 10 } }),
        view = { entries = { name = "wildmenu", separator = " | " } },
      })
      cmp.setup.cmdline(":", {
        mapping = cmp.mapping.preset.cmdline({
          ["<C-j>"] = cmp.mapping({ c = function() vim.api.nvim_feedkeys(t("<Down>"), "n", true) end, }),
          ["<C-k>"] = cmp.mapping({ c = function() vim.api.nvim_feedkeys(t("<Up>"), "n", true) end, }),
          ["<C-y>"] = cmp.mapping({
            c = function(fallback)
              if cmp.visible() then
                if not cmp.get_selected_entry() then
                  cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
                end
                cmp.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false, })
              elseif ls.expand_or_locally_jumpable() then
                ls.expand_or_jump()
              else
                fallback()
              end
            end,
          }),
        }),
        sources = cmp.config.sources(
          { { name = "path", keyword_length = 2, max_item_count = 20 } },
          { { name = "cmdline", keyword_length = 1, max_item_count = 30 } }
        ),
        view = { entries = { name = "custom" } },
      })
      return {
        enabled = function()
          local ctx = require("cmp.config.context")
          if vim.api.nvim_get_mode().mode == "c" then
            return true
          elseif vim.bo.buftype == "prompt" then
            return false
          else
            return not ( ctx.in_treesitter_capture("comment") and ctx.in_syntax_group("Comment"))
          end
        end,
        snippet = { expand = function(args) ls.lsp_expand(args.body) end },
        mapping = cmp.mapping.preset.insert({
          ["<C-b>"] = cmp.mapping.scroll_docs(-4),
          ["<C-f>"] = cmp.mapping.scroll_docs(4),
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<C-e>"] = cmp.mapping.abort(),
          ["<C-y>"] = cmp.mapping({
            i = function(fallback)
              if cmp.visible() then
                if not cmp.get_selected_entry() then
                  cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
                end
                cmp.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false })
              elseif ls.expand_or_locally_jumpable() then
                ls.expand_or_jump()
              else
                fallback()
              end
            end,
          }),
        }),
        window = {
          completion = cmp.config.window.bordered({ scrollbar = true, border = S.Border }),
          documentation = cmp.config.window.bordered({ scrollbar = true, border = S.Border }),
        },
        sources = cmp.config.sources({
          { name = "nvim_lsp", keyword_length = 1, max_item_count = 10, priority = 1000,
            option = { markdown_oxide = { keyword_pattern = [[\(\k\| \|\/\|#\)\+]] } } } },
          { { name = "nvim_lsp_signature_help", keyword_length = 2, max_item_count = 10, priority = 850 } },
          { { name = "luasnip", keyword_length = 2, max_item_count = 10, priority = 850 } },
          { { name = "nvim_lua", keyword_length = 3, max_item_count = 10, priority = 800 } },
          { { name = "buffer", keyword_length = 2, max_item_count = 10, priority = 800 } },
          { { name = "path", keyword_length = 2, max_item_count = 10, priority = 550 } },
          { { name = "treesitter", keyword_length = 2, max_item_count = 10, priority = 525 } },
          { { name = "vimtex", keyword_length = 2, max_item_count = 10, priority = 400 } }),
        -- stylua: ignore end
        formatting = {
          fields = { "kind", "abbr", "menu" },
          format = function(entry, item)
            if vim.tbl_contains({ "path" }, entry.source.name) then
              local icon, hl_group =
                devicon.get_icon(entry:get_completion_item().label)
              if icon then
                item.kind = icon
                item.kind_hl_group = hl_group
                return item
              end
            end
            return lspkind.cmp_format({
              mode = "symbol",
              maxwidth = function() return math.floor(0.75 * vim.o.columns) end,
              menu = {
                nvim_lsp = "[LSP]",
                nvim_lua = "[Lua]",
                luasnip = "[SNIP]",
                path = "[Path]",
                buffer = "[Buf]",
                cmdline = "[CMD]",
                vimtex = "[Tex]",
              },
            })(entry, item)
          end,
        },
        view = { entries = { name = "custom" } },
      }
    end,
  },
}
