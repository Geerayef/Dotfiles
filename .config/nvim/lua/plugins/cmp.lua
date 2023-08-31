return {
  {
    "L3MON4D3/LuaSnip",
    build = (not jit.os:find("Windows"))
      and "echo 'NOTE: jsregexp is optional, so not a big deal if it fails to build'; make install_jsregexp"
      or nil,
    dependencies = {
      "rafamadriz/friendly-snippets",
      config = function()
        require("luasnip.loaders.from_vscode").lazy_load()
      end,
    },
    opts = {
      history = true,
      delete_check_events = "TextChanged",
    },
    keys = {
      {
        "<tab>",
        function()
          return require("luasnip").jumpable(1) and "<Plug>luasnip-jump-next" or "<tab>"
        end,
        expr = true, silent = true, mode = "i",
      },
      { "<tab>", function() require("luasnip").jump(1) end, mode = "s" },
      { "<s-tab>", function() require("luasnip").jump(-1) end, mode = { "i", "s" } },
    },
  },
  {
    "hrsh7th/nvim-cmp",
    version = false,
    event = "InsertEnter",
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-cmdline",
      "saadparwaiz1/cmp_luasnip",
      "onsails/lspkind.nvim",
    },
    opts = function()
      vim.api.nvim_set_hl(0, "CmpGhostText", { link = "Comment", default = true })
      local cmp = require("cmp")
      local lspkind = require("lspkind")
      local luasnip = require("luasnip")

      local t = function(str)
        return vim.api.nvim_replace_termcodes(str, true, true, true)
      end

      -- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
      cmp.setup.cmdline({ '/', '?' }, {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = "buffer" }
        },
        window = {
          completion = cmp.config.window.bordered({
            winhighlight = "Normal:CmpPmenu",
          }),
        },
        view = {
          entries = { name = "wildmenu", separator = ' | ' }
        },
      })

      -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
      cmp.setup.cmdline(':', {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources(
          {
            { name = "path" }
          },
          {
            { name = "cmdline" }
          }
        ),
        window = {
          completion = cmp.config.window.bordered({
            winhighlight = "Normal:CmpPmenu",
          }),
        },
        view = {
          entries = { name = "custom" }
        }
      })

      return {
        enabled = function()
          -- disable completion in comments
          local context = require 'cmp.config.context'
          -- keep command mode completion enabled when cursor is in a comment
          if vim.api.nvim_get_mode().mode == 'c' then
            return true
          else
            return not context.in_treesitter_capture("comment")
              and not context.in_syntax_group("Comment")
          end
        end,
        snippet = {
          expand = function(args)
            luasnip.lsp_expand(args.body)
          end,
        },
        mapping = cmp.mapping.preset.insert({
          ["<C-j>"] = cmp.mapping({
            c = function()
              if cmp.visible() then
                cmp.select_next_item()
              else
                vim.api.nvim_feedkeys(t("<Down>"), "n", true)
              end
            end,
            i = function(fallback)
              if cmp.visible() then
                cmp.select_next_item()
              else
                fallback()
              end
            end
          }),
          ["<C-k>"] = cmp.mapping({
            c = function()
              if cmp.visible() then
                cmp.select_prev_item()
              else
                vim.api.nvim_feedkeys(t("<Up>"), "n", true)
              end
            end,
            i = function(fallback)
              if cmp.visible() then
                cmp.select_prev_item()
              else
                fallback()
              end
            end
          }),
          ["<C-b>"] = cmp.mapping.scroll_docs(-4),
          ["<C-f>"] = cmp.mapping.scroll_docs(4),
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<C-e>"] = cmp.mapping.close(),
          ["<CR>"] = cmp.mapping.confirm({
            behavior = cmp.ConfirmBehavior.Replace,
            select = false,
          }),
        }),
        window = {
          completion = cmp.config.window.bordered({
            scrollbar = false,
            winhighlight = "Normal:CmpPmenu",
          }),
          documentation = cmp.config.window.bordered({})
        },
        sources = cmp.config.sources({
          { name = "nvim_lsp" },
          { name = "nvim_lsp_signature_help" },
          { name = "nvim_lua" },
          { name = "luasnip" },
          { name = "path" },
          { name = "buffer" },
          { name = "cmdline" },
        }),
        completion = {
          keyword_length = 2,
        },
        formatting = {
          fields = { "kind", "abbr", "menu" },
          format = function (entry, item)
            if vim.tbl_contains({ 'path' }, entry.source.name) then
              local icon, hl_group = require('nvim-web-devicons').get_icon(entry:get_completion_item().label)
              if icon then
                item.kind = icon
                item.kind_hl_group = hl_group
                return item
              end
            end
            return lspkind.cmp_format({
              with_text = false,
              mode = "symbol",
              maxwidth = 50,
              menu = ({
                nvim_lsp = "[LSP]",
                nvim_lua = "[Lua]",
                luasnip = "[SNIP]",
                path = "[Path]",
                buffer = "[Buffer]",
                cmdline = "[CMD]",
              }),
            })(entry, item)
            -- local strings = vim.split(kind.kind, "%s", { trimempty = true })
            -- kind.kind = " " .. (strings[1] or "") .. " "
            -- kind.menu = "    (" .. (strings[2] or "") .. ")"
            -- return kind
          end,
        },
        view = ({
          entries = { name = "custom", selection_order = "near_cursor" }
        })
      }
    end,
  },
}
