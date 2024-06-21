---@diagnostic disable
version = "0.21.9"
local xplr = xplr
---@diagnostic enable

local g = xplr.config.general
local l = xplr.config.layouts

l.builtin.default.Horizontal.config = {
  margin = nil,
  horizontal_margin = nil,
  vertical_margin = nil,
  constraints = { { Percentage = 60 }, { Percentage = 40 } },
}

g.start_fifo = nil
g.show_hidden = true
g.initial_mode = "default"
g.initial_layout = "default"
g.search.algorithm = "Fuzzy"
g.search.unordered = false

-- ~ Prompt----------------------------------------------------------------- ~ --

g.prompt.format = "❯ "
g.prompt.style = {}

-- ~ Log----------------------------------------------------------------- ~ --

g.logs.info.format = "INFO"
g.logs.info.style = { fg = "LightBlue" }
g.logs.success.format = "SUCCESS"
g.logs.success.style = { fg = "Green" }
g.logs.warning.format = "WARNING"
g.logs.warning.style = { fg = "Yellow" }
g.logs.error.format = "ERROR"
g.logs.error.style = { fg = "Red" }

-- ~ Table----------------------------------------------------------------- ~ --

g.table.header.cols = {
  { format = " index", style = {} },
  { format = "┌─── path", style = {} },
  { format = "perm", style = {} },
  { format = "size", style = {} },
  { format = "modified", style = {} },
}
g.table.header.style = {}
g.table.header.height = 1
g.table.row.cols = {
  { format = "builtin.fmt_general_table_row_cols_0", style = {} },
  { format = "builtin.fmt_general_table_row_cols_1", style = {} },
  { format = "builtin.fmt_general_table_row_cols_2", style = {} },
  { format = "builtin.fmt_general_table_row_cols_3", style = {} },
  { format = "builtin.fmt_general_table_row_cols_4", style = {} },
}
g.table.row.style = {}
g.table.row.height = 0
g.table.style = {}
g.table.tree = {
  { format = "├", style = {} },
  { format = "├", style = {} },
  { format = "└", style = {} },
}
g.table.col_spacing = 1
g.table.col_widths = {
  { Percentage = 10 },
  { Percentage = 50 },
  { Percentage = 10 },
  { Percentage = 10 },
  { Percentage = 20 },
}

g.selection.item.format = "builtin.fmt_general_selection_item"
g.selection.item.style = {}

-- ~ UI--------------------------------------------------------------------- ~ --

g.default_ui.prefix = "  "
g.default_ui.suffix = ""
g.default_ui.style = {}
g.focus_ui.prefix = "▸["
g.focus_ui.suffix = "]"
g.focus_ui.style = { add_modifiers = { "Bold" } }
g.selection_ui.prefix = " {"
g.selection_ui.suffix = "}"
g.selection_ui.style = {
  fg = "DarkGray",
  add_modifiers = { "CrossedOut" },
}
g.focus_selection_ui.prefix = "▸["
g.focus_selection_ui.suffix = "]"
g.focus_selection_ui.style = {
  add_modifiers = { "Bold", "CrossedOut" },
}
g.panel_ui.default.title.format = nil
g.panel_ui.default.title.style = {
  fg = "Reset",
  add_modifiers = { "Bold" },
}
g.panel_ui.default.style = {}
g.panel_ui.default.borders = { "Top", "Right", "Bottom", "Left" }
g.panel_ui.default.border_type = "Plain"
g.panel_ui.default.border_style = { fg = "DarkGray" }

-- The default layout
xplr.config.layouts.builtin.default = {
  Horizontal = {
    config = {
      constraints = {
        { Percentage = 70 },
        { Percentage = 30 },
      },
    },
    splits = {
      {
        Vertical = {
          config = {
            constraints = {
              { Length = 3 },
              { Min = 1 },
              { Length = 3 },
            },
          },
          splits = {
            "SortAndFilter",
            "Table",
            "InputAndLogs",
          },
        },
      },
      {
        Vertical = {
          config = {
            constraints = {
              { Percentage = 30 },
              { Percentage = 70 },
            },
          },
          splits = {
            "Selection",
            "HelpMenu",
          },
        },
      },
    },
  },
}

-- The layout without help menu
xplr.config.layouts.builtin.no_help = {
  Horizontal = {
    config = {
      constraints = {
        { Percentage = 70 },
        { Percentage = 30 },
      },
    },
    splits = {
      {
        Vertical = {
          config = {
            constraints = {
              { Length = 3 },
              { Min = 1 },
              { Length = 3 },
            },
          },
          splits = {
            "SortAndFilter",
            "Table",
            "InputAndLogs",
          },
        },
      },
      "Selection",
    },
  },
}

-- The layout without help menu and selection panel
xplr.config.layouts.builtin.no_help_no_selection = {
  Vertical = {
    config = {
      constraints = {
        { Length = 3 },
        { Min = 1 },
        { Length = 3 },
      },
    },
    splits = {
      "SortAndFilter",
      "Table",
      "InputAndLogs",
    },
  },
}
