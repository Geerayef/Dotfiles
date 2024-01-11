return {
  {
    "L3MON4D3/LuaSnip",
    build = (not jit.os:find("Windows")) and "echo 'NOTE: jsregexp is optional'; make install_jsregexp" or nil,
    lazy = true,
    event = { "InsertEnter" },
    dependencies = {
      "rafamadriz/friendly-snippets",
      config = function() require("luasnip.loaders.from_vscode").lazy_load() end,
    },
    opts = { history = true, delete_check_events = "TextChanged" },
    keys = {
      {
        "<tab>",
        function() return require("luasnip").jumpable(1) and "<Plug>luasnip-jump-next" or "<tab>" end,
        expr = true,
        silent = true,
        mode = "i",
      },
      { "<tab>", function() require("luasnip").jump(1) end, mode = "s" },
      { "<s-tab>", function() require("luasnip").jump(-1) end, mode = { "i", "s" } },
    },
  },
  {
    "hrsh7th/nvim-cmp",
    version = false,
    lazy = true,
    event = { "BufReadPost", "InsertEnter", "UiEnter" },
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-nvim-lsp-signature-help",
      "hrsh7th/cmp-cmdline",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "saadparwaiz1/cmp_luasnip",
      "onsails/lspkind.nvim",
    },
    opts = function()
      local cmp = require("cmp")
      local lspkind = require("lspkind")
      local luasnip = require("luasnip")
      local t = function(str) return vim.api.nvim_replace_termcodes(str, true, true, true) end
      -- "buffer" source for '/', '?'. "cmdline" and "path" source for ':'
      -- NOTE: If you enable 'native_menu', this won't work anymore
      cmp.setup.cmdline({ "/", "?" }, {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({ { name = "buffer" } }),
        view = { entries = { name = "wildmenu", separator = " | " } },
      })
      cmp.setup.cmdline(":", {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({ { name = "path" } }, { { name = "cmdline" } }),
        view = { entries = { name = "custom" } },
      })
      return {
        enabled = function()
          local context = require("cmp.config.context")
          if vim.api.nvim_get_mode().mode == "c" then
            return true
          elseif vim.bo.buftype == "prompt" then
            return false
          else
            return not context.in_treesitter_capture("comment") and not context.in_syntax_group("Comment")
          end
        end,
        snippet = { expand = function(args) luasnip.lsp_expand(args.body) end },
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
            end,
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
            end,
          }),
          ["<C-b>"] = cmp.mapping.scroll_docs(-4),
          ["<C-f>"] = cmp.mapping.scroll_docs(4),
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<C-e>"] = cmp.mapping.close(),
          ["<CR>"] = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false }),
        }),
        window = {
          -- winhighlight = "Normal:Pmenu"
          completion = cmp.config.window.bordered({ scrollbar = false }),
          documentation = cmp.config.window.bordered(),
        },
        sources = cmp.config.sources(
          { { name = "nvim_lsp" }, { name = "nvim_lsp_signature_help" } },
          { { name = "buffer" }, { name = "path" }, { name = "luasnip" } },
          { { name = "nvim_lua" } },
          { { name = "treesitter" } }
        ),
        -- completion = { keyword_length = 2 },
        formatting = {
          fields = { "kind", "abbr", "menu" },
          format = function(entry, item)
            if vim.tbl_contains({ "path" }, entry.source.name) then
              local icon, hl_group = require("nvim-web-devicons").get_icon(entry:get_completion_item().label)
              if icon then
                item.kind = icon
                item.kind_hl_group = hl_group
                return item
              end
            end
            return lspkind.cmp_format({
              mode = "symbol",
              maxwidth = 50,
              menu = {
                nvim_lsp = "[LSP]",
                nvim_lua = "[Lua]",
                luasnip = "[SNIP]",
                path = "[Path]",
                buffer = "[Buffer]",
                cmdline = "[CMD]",
              },
            })(entry, item)
          end,
        },
        -- selection_order = "near_cursor"
        view = { entries = { name = "custom" } },
      }
    end,
  },
}
