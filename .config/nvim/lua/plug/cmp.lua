-- ~ nvim-cmp {{{
-- {
--   "hrsh7th/nvim-cmp",
--   version = false,
--   lazy = true,
--   dependencies = {
--     "hrsh7th/cmp-nvim-lsp",
--     "hrsh7th/cmp-nvim-lsp-signature-help",
--     "hrsh7th/cmp-nvim-lua",
--     "hrsh7th/cmp-cmdline",
--     "hrsh7th/cmp-buffer",
--     "amarakon/nvim-cmp-buffer-lines",
--     { url = "https://codeberg.org/FelipeLema/cmp-async-path" },
--     {
--       "L3MON4D3/LuaSnip",
--       build = "make install_jsregexp",
--       opts = { delete_check_events = "TextChanged" },
--     },
--     {
--       "rafamadriz/friendly-snippets",
--       config = function() require("luasnip.loaders.from_vscode").lazy_load() end,
--     },
--     { "onsails/lspkind.nvim" },
--     { "saadparwaiz1/cmp_luasnip" },
--   },
--   opts = function()
--     local ls = require("luasnip")
--     local lspkind = require("lspkind")
--     local cmp = require("cmp")
--     local t = function(str)
--       return vim.api.nvim_replace_termcodes(str, true, true, true)
--     end
--
--     -- Search
--     -- stylua: ignore start
--     cmp.setup.cmdline({ "/", "?" }, {
--       mapping = cmp.mapping.preset.cmdline(),
--       sources = cmp.config.sources({
--         { name = "buffer", keyword_length = 1, max_item_count = 10 },
--         { name = "buffer-lines" }}),
--       view = { entries = { name = "wildmenu", separator = " │ " } },
--     })
--
--     -- Command
--     cmp.setup.cmdline(":", {
--       mapping = cmp.mapping.preset.cmdline({
--         ["<C-j>"] = cmp.mapping({ c = function() vim.api.nvim_feedkeys(t("<Down>"), "n", true) end, }),
--         ["<C-k>"] = cmp.mapping({ c = function() vim.api.nvim_feedkeys(t("<Up>"), "n", true) end, }),
--         ["<C-y>"] = cmp.mapping({ c = function()
--           if cmp.visible() then
--             if not cmp.get_selected_entry() then
--               cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
--             end
--             cmp.confirm({ select = true })
--           else
--             cmp.complete()
--           end
--         end }),
--       }),
--       sources = cmp.config.sources(
--         { { name = "cmdline", keyword_length = 1, max_item_count = 20 } },
--         { { name = "async_path", keyword_length = 2, max_item_count = 20 } }
--       ),
--       view = { entries = { name = "custom" } },
--     })
--     -- stylua: ignore end
--
--     -- Main
--     return {
--       enabled = function()
--         local ctx = require("cmp.config.context")
--         if vim.api.nvim_get_mode().mode == "c" then
--           return true
--         elseif vim.bo.buftype == "prompt" then
--           return false
--         else
--           return not (
--             ctx.in_treesitter_capture("comment")
--             and ctx.in_syntax_group("Comment")
--           )
--         end
--       end,
--       snippet = { expand = function(args) ls.lsp_expand(args.body) end },
--       completion = { autocomplete = false },
--       performance = { debounce = 10, throttle = 20 },
--       sorting = {
--         priority_weight = 4,
--         comparators = {
--           cmp.config.compare.exact,
--           cmp.config.compare.scopes,
--           cmp.config.compare.offset,
--           cmp.config.compare.locality,
--           cmp.config.compare.score,
--           cmp.config.compare.length,
--           cmp.config.compare.kind,
--           cmp.config.compare.order,
--           -- cmp.config.compare.sort_text,
--           -- cmp.config.compare.recently_used,
--         },
--       },
--       -- stylua: ignore start
--       -- Source groups: LSP & snippet, buffer, path, vimtex.
--       sources = cmp.config.sources(
--         { { name = "nvim_lsp",                keyword_length = 0, max_item_count = 20, priority = 900, group_index = 1,
--             option = { markdown_oxide = { keyword_pattern = [[\(\k\| \|\/\|#\)\+]] } } },
--           { name = "nvim_lsp_signature_help", keyword_length = 1, max_item_count = 20, priority = 875, group_index = 1, },
--           { name = "luasnip",                 keyword_length = 2, max_item_count =  5, priority = 800, group_index = 1, },
--           { name = "nvim_lua",                keyword_length = 3, max_item_count = 10, priority = 600, group_index = 1, } },
--         { { name = "buffer",                  keyword_length = 3, max_item_count = 10, priority = 850, group_index = 2 },
--           { name = "buffer-lines",            keyword_length = 4, max_item_count = 10, priority = 825, group_index = 2,
--             option = { comments = true, leading_whitespace = false } } },
--         { { name = "async_path",              keyword_length = 2, max_item_count = 10, priority = 750, group_index = 3,
--             option = { trailing_slash = true, show_hidden_files_by_default = true } } },
--         { { name = "vimtex",                  keyword_length = 2, max_item_count = 20, priority = 500 } }
--         -- { { name = "treesitter", keyword_length = 2, max_item_count = 20, priority = 600 } }
--       ),
--       window = {
--         completion = cmp.config.window.bordered({ scrollbar = true, border = S.Border }),
--         documentation = cmp.config.window.bordered({ scrollbar = true, border = S.Border }),
--       },
--       mapping = {
--         -- ["<C-x><C-l>"] = function() if cmp.visible() then end end,
--         ["<C-n>"] = function()
--           if cmp.visible() then cmp.select_next_item({ behaviour = cmp.SelectBehavior.Select }) else cmp.complete() end
--         end,
--         ["<C-p>"] = function()
--           if cmp.visible() then cmp.select_prev_item({ behaviour = cmp.SelectBehavior.Select }) else cmp.complete() end
--         end,
--         ["<C-b>"] = cmp.mapping.scroll_docs(-4),
--         ["<C-f>"] = cmp.mapping.scroll_docs(4),
--         ["<C-Space>"] = function()
--           if not cmp.visible() then cmp.complete() else cmp.complete_common_string() end
--         end,
--         ["<C-e>"] = cmp.mapping.abort(),
--         ["<C-y>"] = cmp.mapping({
--           i = function(fb)
--             if cmp.visible() then
--               if not cmp.get_selected_entry() then
--                 cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
--               end
--               cmp.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false })
--             elseif ls.expand_or_locally_jumpable() then
--               ls.expand_or_jump()
--             else
--               fb()
--             end
--           end,
--         }),
--       },
--       -- stylua: ignore end
--       formatting = {
--         fields = { "kind", "abbr", "menu" },
--         format = lspkind.cmp_format({
--           mode = "symbol",
--           show_labelDetails = true,
--           maxwidth = function() return math.floor(0.75 * vim.o.columns) end,
--           menu = {
--             nvim_lsp = "[LSP]",
--             nvim_lua = "[Vim]",
--             luasnip = "[Snip]",
--             async_path = "[Path]",
--             buffer = "[Buf]",
--             ["buffer-lines"] = "[Line]",
--             cmdline = "[CMD]",
--             vimtex = "[Tex]",
--           },
--         },
--       },
--     }
--   end,
-- },
-- }}}
return {
  -- ~ nvim-cmp[cmdline] {{{
  {
    "hrsh7th/nvim-cmp",
    version = false,
    event = "CmdlineEnter",
    dependencies = "hrsh7th/cmp-cmdline",
    opts = function()
      local cmp = require("cmp")
      -- stylua: ignore start
      local t = function(str) return vim.api.nvim_replace_termcodes(str, true, true, true) end
      cmp.setup.cmdline({ "/", "?" }, {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({ { name = "buffer" } }),
        view = { entries = { name = "wildmenu", separator = " │ " } },
      })
      cmp.setup.cmdline(":", {
        mapping = cmp.mapping.preset.cmdline({
          ["<C-j>"] = cmp.mapping({ c = function() vim.api.nvim_feedkeys(t("<Down>"), "n", true) end, }),
          ["<C-k>"] = cmp.mapping({ c = function() vim.api.nvim_feedkeys(t("<Up>"), "n", true) end, }),
          ["<C-y>"] = cmp.mapping({ c = function()
            if cmp.visible() then
              if not cmp.get_selected_entry() then cmp.select_next_item({ behavior = cmp.SelectBehavior.Select }) end
              cmp.confirm({ select = true })
            else cmp.complete() end
          end }),
        }),
        sources = cmp.config.sources({ { name = "cmdline" } }),
        view = { entries = { name = "custom" } },
      })
      return {
        window = {
          completion = cmp.config.window.bordered({ scrollbar = true, border = S.Border }),
          documentation = cmp.config.window.bordered({ scrollbar = true, border = S.Border }),
        },
      }
      -- stylua: ignore end
    end,
  },
  -- }}}
  -- { "saghen/blink.compat", version = "v2.*", lazy = false, opts = { impersonate_nvim_cmp = true } },
  -- ~ blink.cmp {{{
  {
    "saghen/blink.cmp",
    lazy = false,
    dependencies = { "rafamadriz/friendly-snippets" },
    version = "v0.*",
    opts = {
      -- show, hide, cancel, accept, select_and_accept, select_prev, select_next, show_documentation, hide_documentation,
      -- scroll_documentation_up, scroll_documentation_down, snippet_forward, snippet_backward, fallback
      keymap = {
        ["<C-n>"] = { "show", "select_next", "fallback" },
        ["<C-p>"] = { "show", "select_prev", "fallback" },
        ["<C-b>"] = { "scroll_documentation_up" },
        ["<C-f>"] = { "scroll_documentation_down" },
        ["<C-Space>"] = { "show" },
        ["<C-'>"] = { "show_documentation", "hide_documentation" },
        ["<C-e>"] = { "cancel", "hide" },
        ["<C-y>"] = { "accept", "select_and_accept" },
        ["<Tab>"] = { "snippet_forward", "fallback" },
        ["<S-Tab>"] = { "snippet_backward", "fallback" },
      },
      -- ~ Completion {{{
      completion = {
        keyword = {
          range = "prefix", -- prefix | full
          regex = "[%w_\\-]",
          exclude_from_prefix_regex = "[\\-]",
        },
        trigger = {
          show_in_snippet = false,
          show_on_keyword = false,
          show_on_trigger_character = false,
          show_on_blocked_trigger_characters = {}, --[[ { " ", "\n", "\t" }, ]]
          show_on_accept_on_trigger_character = false,
          show_on_insert_on_trigger_character = false,
          show_on_x_blocked_trigger_characters = {}, --[[ { "'", "\"", "(" }, ]]
        },
        list = {
          max_items = 50,
          selection = "manual", -- preselect | manual | auto_insert
          cycle = { from_bottom = true, from_top = true },
        },
        accept = {
          create_undo_point = false,
          auto_brackets = {
            enabled = true,
            default_brackets = { "(", ")" },
            override_brackets_for_filetypes = {},
            kind_resolution = {
              enabled = true,
              blocked_filetypes = {
                "typescriptreact",
                "javascriptreact",
                "vue",
              },
            },
            semantic_token_resolution = {
              enabled = true,
              blocked_filetypes = {},
              timeout_ms = 400,
            },
          },
        },
        menu = {
          enabled = true,
          min_width = 15,
          max_height = 15,
          border = S.Border,
          winblend = 0,
          -- winhighlight = "Normal:BlinkCmpMenu,FloatBorder:BlinkCmpMenuBorder,CursorLine:BlinkCmpMenuSelection,Search:None",
          winhighlight = "BlinkCmpMenu:Normal,BlinkCmpMenuBorder:FloatBorder,BlinkCmpMenuSelection:CursorLine,None:Search",
          scrolloff = 2,
          scrollbar = true,
          direction_priority = { "s", "n" },
          draw = {
            align_to_component = "label", -- label | none
            padding = 1, -- { left, right }
            gap = 1,
            columns = {
              { "kind_icon" },
              { "label", "label_description", gap = 1 },
            },
            -- ellipsis           : whether to add an ellipsis when truncating the text
            -- width              : control the min, max and fill behavior of the component
            -- text_function      : will be called for each item
            -- highlight_function : will be called only when the line appears on screen
            components = {
              kind_icon = {
                ellipsis = false,
                text = function(ctx) return ctx.kind_icon .. ctx.icon_gap end,
                highlight = function(ctx)
                  return require("blink.cmp.completion.windows.render.tailwind").get_hl(
                    ctx
                  ) or ("BlinkCmpKind" .. ctx.kind)
                end,
              },
              kind = {
                ellipsis = false,
                width = { fill = true },
                text = function(ctx) return ctx.kind end,
                highlight = function(ctx)
                  return require("blink.cmp.completion.windows.render.tailwind").get_hl(
                    ctx
                  ) or ("BlinkCmpKind" .. ctx.kind)
                end,
              },
              label = {
                width = { fill = true, max = 60 },
                text = function(ctx) return ctx.label .. ctx.label_detail end,
                highlight = function(ctx)
                  local highlights = {
                    {
                      0,
                      #ctx.label,
                      group = ctx.deprecated and "BlinkCmpLabelDeprecated"
                        or "BlinkCmpLabel",
                    },
                  }
                  if ctx.label_detail then
                    table.insert(highlights, {
                      #ctx.label,
                      #ctx.label + #ctx.label_detail,
                      group = "BlinkCmpLabelDetail",
                    })
                  end
                  -- characters matched on the label by the fuzzy matcher
                  for _, idx in ipairs(ctx.label_matched_indices) do
                    table.insert(
                      highlights,
                      { idx, idx + 1, group = "BlinkCmpLabelMatch" }
                    )
                  end
                  return highlights
                end,
              },
              label_description = {
                width = { max = 30 },
                text = function(ctx) return ctx.label_description end,
                highlight = "BlinkCmpLabelDescription",
              },
              source_name = {
                width = { max = 30 },
                text = function(ctx) return ctx.source_name end,
                highlight = "BlinkCmpSource",
              },
            },
          },
        },
        documentation = {
          auto_show = true,
          auto_show_delay_ms = 500,
          update_delay_ms = 50,
          treesitter_highlighting = true,
          window = {
            min_width = 10,
            max_width = 60,
            max_height = 20,
            border = S.Border,
            winblend = 0,
            -- winhighlight = "Normal:BlinkCmpDoc,FloatBorder:BlinkCmpDocBorder,CursorLine:BlinkCmpDocCursorLine,Search:None",
            winhighlight = "BlinkCmpDoc:Normal,BlinkCmpDocBorder:FloatBorder,BlinkCmpDocCursorLine:CursorLine,None:Search",
            scrollbar = true,
            direction_priority = {
              menu_north = { "e", "w", "n", "s" },
              menu_south = { "e", "w", "s", "n" },
            },
          },
        },
        ghost_text = { enabled = true },
      },
      -- }}}
      -- ~ Signature {{{
      signature = {
        enabled = true,
        trigger = {
          blocked_trigger_characters = {},
          blocked_retrigger_characters = {},
          show_on_insert_on_trigger_character = true,
        },
        window = {
          min_width = 1,
          max_width = 100,
          max_height = 10,
          border = S.Border,
          winblend = 0,
          winhighlight = "BlinkCmpSignatureHelp:Normal,BlinkCmpSignatureHelpBorder:FloatBorder",
          scrollbar = false,
          direction_priority = { "n", "s" },
          treesitter_highlighting = true,
        },
      },
      -- }}}
      -- ~ Fuzzy {{{
      fuzzy = {
        use_typo_resistance = true,
        use_frecency = true,
        use_proximity = true,
        max_items = 200,
        sorts = { "label", "kind", "score" },
        prebuilt_binaries = {
          download = true,
          force_version = nil,
          force_system_triple = nil,
        },
      },
      -- }}}
      -- ~ Sources {{{
      sources = {
        completion = {
          enabled_providers = { "lsp", "path", "snippets", "buffer" },
          -- enabled_providers = function(ctx)
          --   local node = vim.treesitter.get_node()
          --   if vim.bo.filetype == 'lua' then
          --     return { 'lsp', 'path' }
          --   elseif node and vim.tbl_contains({ 'comment', 'line_comment', 'block_comment' }), node:type())
          --     return { 'buffer' }
          --   else
          --     return { 'lsp', 'path', 'snippets', 'buffer' }
          --   end
          -- end
        },
        -- Please see https://github.com/Saghen/blink.compat for using `nvim-cmp` sources
        providers = {
          lsp = {
            name = "LSP",
            module = "blink.cmp.sources.lsp",
            enabled = true,
            transform_items = nil,
            should_show_items = true,
            max_items = 25,
            min_keyword_length = 0,
            fallback_for = {},
            score_offset = 0,
            override = nil,
          },
          path = {
            name = "Path",
            module = "blink.cmp.sources.path",
            min_keyword_length = 3,
            score_offset = 3,
            opts = {
              trailing_slash = false,
              label_trailing_slash = true,
              get_cwd = function(context)
                return vim.fn.expand(("#%d:p:h"):format(context.bufnr))
              end,
              show_hidden_files_by_default = false,
            },
          },
          snippets = {
            name = "Snippets",
            module = "blink.cmp.sources.snippets",
            score_offset = -3,
            min_keyword_length = 2,
            opts = {
              friendly_snippets = true,
              search_paths = { vim.fn.stdpath("config") .. "/snippets" },
              global_snippets = { "all" },
              extended_filetypes = {},
              ignored_filetypes = {},
              get_filetype = function(ctx) return vim.bo.filetype end,
            },
            --- Example usage for disabling the snippet provider after pressing trigger characters (i.e. ".")
            -- enabled = function(ctx)
            --   return ctx ~= nil and ctx.trigger.kind == vim.lsp.protocol.CompletionTriggerKind.TriggerCharacter
            -- end,
          },
          buffer = {
            name = "Buffer",
            module = "blink.cmp.sources.buffer",
            min_keyword_length = 1,
            fallback_for = { "lsp" },
            opts = {
              -- default to all visible buffers
              get_bufnrs = function()
                return vim
                  .iter(vim.api.nvim_list_wins())
                  :map(function(win) return vim.api.nvim_win_get_buf(win) end)
                  :filter(
                    function(buf) return vim.bo[buf].buftype ~= "nofile" end
                  )
                  :totable()
              end,
            },
          },
          -- cmdline = {
          --   name = "cmdline",
          --   module = "blink.compat.source",
          --   min_keyword_length = 1,
          -- },
        },
      },
      -- }}}
      -- ~ Appearance {{{
      appearance = {
        highlight_ns = vim.api.nvim_create_namespace("blink_cmp"),
        use_nvim_cmp_as_default = true,
        nerd_font_variant = "mono",
        kind_icons = {
          Text = "󰉿",
          Method = "󰊕",
          Function = "󰊕",
          Constructor = "󰒓",
          Field = "󰜢",
          Variable = "󰆦",
          Property = "󰖷",
          Class = "󱡠",
          Interface = "󱡠",
          Struct = "󱡠",
          Module = "󰅩",
          Unit = "󰪚",
          Value = "󰦨",
          Enum = "󰦨",
          EnumMember = "󰦨",
          Keyword = "󰻾",
          Constant = "󰏿",
          Snippet = "󱄽",
          Color = "󰏘",
          File = "󰈔",
          Reference = "󰬲",
          Folder = "󰉋",
          Event = "󱐋",
          Operator = "󰪚",
          TypeParameter = "󰬛",
        },
      },
      -- }}}
    },
    opts_extend = { "sources.completion.enabled_providers" },
  },
  -- }}}
}
