local ui = S.Icons.ui
return {
  {
    "obsidian-nvim/obsidian.nvim",
    version = "*",
    ft = "markdown",
    dependencies = {
      {
        "MeanderingProgrammer/render-markdown.nvim",
        ft = { "markdown", "codecompanion" },
        opts = {
          preset = "obsidian",
          completions = { lsp = { enabled = true } },
          render_modes = true,
          bullet = { icons = { S.Icons.ui.dot }, right_pad = 1 },
        },
      },
    },
    opts = {
      workspaces = {
        { name = "notes", path = "~/notes", strict = true },
        {
          name = "self",
          path = "~/notes/self",
          strict = true,
          overrides = { notes_subdir = "" },
        },
        {
          name = "dev",
          path = "~/notes/dev",
          strict = true,
          overrides = { notes_subdir = "" },
        },
      },
      daily_notes = {
        folder = "daily",
        date_format = "%d-%m-%Y",
        alias_format = "%B %-d, %Y",
        template = nil,
      },
      templates = {
        folder = "template",
        date_format = "%d-%m-%Y",
        time_format = "%H:%M",
        -- `substitutions`: map for custom variables. `key`: variable, `value`: function.
        substitutions = {},
      },
      legacy_commands = false,
      frontmatter = { enabled = false },
      footer = { enabled = true },
      completion = { blink = true, nvim_cmp = false, min_chars = 2 },
      follow_url_func = function(url) vim.system({ "xdg-open", url }) end,
      preferred_link_style = "markdown",
      sort_by = "path",
      sort_reversed = false,
      picker = { name = "fzf-lua" },
      checkbox = {
        enabled = true,
        create_new = true,
        order = { " ", "x", ">", "!", "-" },
      },
      ui = {
        enable = false,
        update_debounce = 200,
        checkboxes = {
          [" "] = { char = ui.box_empty, hl_group = "ObsidianTodo" },
          ["x"] = { char = ui.box_check, hl_group = "ObsidianDone" },
          [">"] = { char = ui.circle_right, hl_group = "ObsidianRightArrow" },
          ["!"] = { char = ui.diamond, hl_group = "ObsidianTilde" },
          ["-"] = { char = ui.clock, hl_group = "ObsidianRightArrow" },
        },
        bullets = { char = ui.dot, hl_group = "ObsidianBullet" },
        external_link_icon = {
          char = ui.link,
          hl_group = "ObsidianExtLinkIcon",
        },
      },
    },
  },
  -- {
  --   "lervag/vimtex",
  --   dependencies = { "micangl/cmp-vimtex" },
  --   ft = { "tex" },
  --   config = function()
  --     vim.g["tex_flavor"] = "latex" -- how to read tex files
  --     vim.g["tex_indent_items"] = 0 -- turn off enumerate indent
  --     vim.g["tex_indent_brace"] = 0 -- turn off brace indent
  --     -- vim.g['vimtex_view_method'] = 'zathura'     -- main variant with xdotool (requires X11; not compatible with wayland)
  --     vim.g["vimtex_view_method"] = "zathura_simple" -- for variant without xdotool to avoid errors in wayland
  --     vim.g["vimtex_quickfix_mode"] = 0 -- suppress error reporting on save and build
  --     vim.g["vimtex_mappings_enabled"] = 0 -- Ignore mappings
  --     vim.g["vimtex_indent_enabled"] = 0 -- Auto Indent
  --     vim.g["vimtex_syntax_enabled"] = 1 -- Syntax highlighting
  --     vim.g["vimtex_context_pdf_viewer"] = "zathura" -- external PDF viewer run from vimtex menu command
  --     vim.g["vimtex_log_ignore"] = {
  --       "Underfull",
  --       "Overfull",
  --       "specifier changed to",
  --       "Token not allowed in a PDF string",
  --     }
  --   end,
  -- },
}
