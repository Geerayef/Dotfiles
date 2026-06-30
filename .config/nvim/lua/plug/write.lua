local rb = require("clrs.road").base
local i = GRIM.static.icon.ui
return {
  {
    "nvim-orgmode/orgmode",
    version = "*",
    cond = vim.g.vscode == nil,
    event = "VeryLazy",
    ft = "org",
    opts = {
      org_agenda_files = "~/notes/**/*",
      org_default_notes_file = "~/notes/note.org",
      mappings = {
        org_return_uses_meta_return = true,
        org = {
          org_time_stamp = { "<leader>odi.", desc = "[o]rg [d]ate [i]nsert [.]" },
          org_time_stamp_inactive = { "<leader>odI.", desc = "[o]rg [d]ate insert [I]nactive [.]" },
          org_toggle_timestamp_type = { "<leader>odt", desc = "[o]rg [d]ate [t]oggle state" },
          org_toggle_heading = { "<leader>o*", desc = "[o]rg [*] heading toggle" },
          org_insert_heading_respect_content = { "<leader>ohi", desc = "[o]rg [h]eading [i]nsert" },
          org_insert_todo_heading = { "<leader>ohT", desc = "[o]rg [h]eading [T]odo" },
          org_insert_todo_heading_respect_content = {
            "<leader>oht",
            desc = "[o]rg [h]eading [t]odo",
          },
          org_insert_link = { "<leader>oli", desc = "[o]rg [l]ink [i]nsert" },
          org_store_link = { "<leader>ols", desc = "[o]rg [l]ink [s]tore" },
        },
      },
      org_todo_keywords = {
        "TODO(t)",
        "NEXT",
        "WAIT",
        "MEETING",
        "|",
        "CANCELLED",
        "DONE",
      },
      org_todo_keyword_faces = {
        TODO = table.concat({ ":foreground", rb.lotusYellow, ":weight", "bold" }, " "),
        NEXT = table.concat({ ":foreground", rb.cadetGray, ":slant", "italic" }, " "),
        WAIT = table.concat({ ":foreground", rb.rustyRed, ":underline", "on" }, " "),
        MEETING = table.concat({ ":foreground", rb.gunmetal }, " "),
        CANCELLED = table.concat({ ":foreground", rb.mintCream }, " "),
        DONE = table.concat({ ":foreground", rb.emerald }, " "),
      },
      org_hide_emphasis_markers = true,
      org_ellipsis = i.ellipsis,
      org_log_into_drawer = "LOGBOOK",
      org_cycle_separator_lines = 1,
      org_capture_templates = {
        d = {
          description = "Daily",
          template = "* %<%B %-d, %Y>\n\n** %?",
          target = "~/notes/daily/%<%Y-%m-%d>.org",
        },
        T = "Task",
        Tf = {
          description = "Task: feature",
          template = "* %^{Title}\n\t%u\n\t%?\n\n** TODO [/]\n\t- [ ] \n\n** Deliverables",
          target = "~/notes/task/Phoenix.%<%Y.%m.%d>_feat_%^{File:Name}.org",
        },
        Tt = {
          description = "Task: test",
          template = "* %^{Title}\n\t%u\n\t%?\n\n** TODO [/]\n\t- [ ] \n\n** Deliverables",
          target = "~/notes/task/Phoenix.%<%Y.%m.%d>_test_%^{File:Name}.org",
        },
        Tx = {
          description = "Task: fix",
          template = "* %^{Title}\n\t%u\n\t%?\n\n** TODO [/]\n\t- [ ] \n\n** Deliverables",
          target = "~/notes/task/Phoenix.%<%Y.%m.%d>_fix_%^{File:Name}.org",
        },
        Tc = {
          description = "Task: chore",
          template = "* %^{Title}\n\t%u\n\t%?\n\n** TODO [/]\n\t- [ ] \n\n** Deliverables",
          target = "~/notes/task/Phoenix.%<%Y.%m.%d>_chore_%^{File:Name}.org",
        },
      },
    },
  },
  {
    "MeanderingProgrammer/render-markdown.nvim",
    cond = vim.g.vscode == nil,
    ft = "markdown",
    opts = {
      preset = "none",
      completions = { lsp = { enabled = true }, blink = { enabled = true } },
      render_modes = true,
      bullet = { icons = { i.dot }, right_pad = 1 },
      checkbox = {
        custom = {
          working = { raw = "[>]", rendered = i.angle_right_l .. " " },
          important = { raw = "[!]", rendered = i.exclamation .. " " },
        },
      },
      yaml = { enabled = true, render_modes = true },
    },
  },
  {
    "hedyhli/markdown-toc.nvim",
    cond = vim.g.vscode == nil,
    cmd = "Mtoc",
    opts = {
      toc_list = { markers = "-" },
      fences = { start_text = "TOC start", end_text = "TOC end" },
    },
  },
  -- {
  --   "lervag/vimtex",
  --   dependencies = { "micangl/cmp-vimtex" },
  --   ft = { "tex" },
  --   config = function()
  --     -- how to read tex files
  --     vim.g["tex_flavor"] = "latex"
  --     -- turn off enumerate indent
  --     vim.g["tex_indent_items"] = 0
  --     -- turn off brace indent
  --     vim.g["tex_indent_brace"] = 0
  --     -- main variant with xdotool (requires X11; not compatible with wayland)
  --     -- vim.g['vimtex_view_method'] = 'zathura'
  --     -- for variant without xdotool to avoid errors in wayland
  --     vim.g["vimtex_view_method"] = "zathura_simple"
  --     -- suppress error reporting on save and build
  --     vim.g["vimtex_quickfix_mode"] = 0
  --     -- Ignore mappings
  --     vim.g["vimtex_mappings_enabled"] = 0
  --     -- Auto Indent
  --     vim.g["vimtex_indent_enabled"] = 0
  --     -- Syntax highlighting
  --     vim.g["vimtex_syntax_enabled"] = 1
  --     -- external PDF viewer run from vimtex menu command
  --     vim.g["vimtex_context_pdf_viewer"] = "zathura"
  --     vim.g["vimtex_log_ignore"] = {
  --       "Underfull",
  --       "Overfull",
  --       "specifier changed to",
  --       "Token not allowed in a PDF string",
  --     }
  --   end,
  -- },
}
