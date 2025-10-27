return {
  "saghen/blink.cmp",
  event = { "InsertEnter", "CmdlineEnter" },
  dependencies = {
    "L3MON4D3/LuaSnip",
    version = "v2.*",
    build = "make install_jsregexp",
    dependencies = "rafamadriz/friendly-snippets",
    config = function() require("luasnip.loaders.from_vscode").lazy_load() end,
  },
  version = "1.*",
  opts = {
    cmdline = {
      keymap = {
        ["<Tab>"] = { "show_and_insert", "select_next" },
        ["<S-Tab>"] = { "show_and_insert", "select_prev" },
        ["<C-n>"] = { "select_next", "fallback" },
        ["<C-p>"] = { "select_prev", "fallback" },
        ["<C-Space>"] = { "show_and_insert", "fallback" },
        ["<C-'>"] = { "show_documentation", "hide_documentation" },
        ["<Esc>"] = { "cancel", "hide" },
        ["<C-y>"] = { "accept", "select_and_accept" },
      },
    },
    keymap = {
      ["<C-n>"] = { "show", "select_next", "fallback" },
      ["<C-p>"] = { "show", "select_prev", "fallback" },
      ["<C-b>"] = { "scroll_documentation_up" },
      ["<C-f>"] = { "scroll_documentation_down" },
      ["<C-Space>"] = { "show" },
      ["<C-'>"] = { "show_documentation", "hide_documentation" },
      ["<C-e>"] = { "cancel", "hide" },
      ["<C-y>"] = { "accept", "select_and_accept", "snippet_forward" },
      ["<Tab>"] = { "snippet_forward", "fallback" },
      ["<S-Tab>"] = { "snippet_backward", "fallback" },
    },
    completion = {
      ghost_text = { enabled = true },
      documentation = {
        auto_show = true,
        window = {
          border = S.Border,
          winhighlight = "BlinkCmpDoc:Normal,BlinkCmpDocBorder:FloatBorder,None:Search",
        },
      },
      trigger = { show_on_keyword = false, show_on_trigger_character = false },
      accept = { dot_repeat = false, create_undo_point = false },
      menu = {
        max_height = 20,
        border = S.Border,
        auto_show = false,
        draw = {
          treesitter = { "lsp" },
          columns = {
            { "kind_icon" },
            { "label", "label_description", gap = 1 },
            { "source_name" },
          },
          components = { kind_icon = { highlight = "Normal" } },
        },
        winhighlight = "BlinkCmpMenu:Normal,BlinkCmpMenuBorder:FloatBorder,BlinkCmpLabelDescription:Normal,BlinkCmpSource:Normal",
      },
    },
    sources = { default = { "lsp", "path", "snippets", "buffer" } },
    snippets = { preset = "luasnip" },
    fuzzy = { implementation = "rust" },
  },
  opts_extend = { "sources.default" },
}
