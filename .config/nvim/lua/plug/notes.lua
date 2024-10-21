return {
  {
    "epwalsh/obsidian.nvim",
    version = "*",
    ft = "markdown",
    dependencies = {
      {
        "MeanderingProgrammer/render-markdown.nvim",
        ft = { "markdown" },
        opts = {
          render_modes = true,
          bullet = { icons = { S.Icons.ui.dot_l }, right_pad = 2 },
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
        {
          name = "LGR",
          path = "~/notes/LGR",
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
      -- `substitutions`: map for custom variables. `key`: variable, `value`: function.
      templates = {
        folder = "template",
        date_format = "%d-%m-%Y",
        time_format = "%H:%M",
        substitutions = {},
      },
      follow_url_func = function(url) vim.system({ "xdg-open", url }) end,
      completion = { nvim_cmp = true, min_chars = 2 },
      preferred_link_style = "markdown",
      disable_frontmatter = true,
      sort_by = "path",
      sort_reversed = false,
      picker = {
        name = "telescope.nvim",
        mappings = { new = "<C-x>", insert_link = "<C-l>" },
      },
      ui = {
        enable = true,
        update_debounce = 200,
        checkboxes = {
          [" "] = { char = "󰄱", hl_group = "ObsidianTodo" },
          ["x"] = { char = "", hl_group = "ObsidianDone" },
          ["-"] = { char = "", hl_group = "ObsidianRightArrow" },
          -- [">"] = { char = "", hl_group = "ObsidianRightArrow" },
          -- ["~"] = { char = "󰰱", hl_group = "ObsidianTilde" },
        },
        bullets = { char = "•", hl_group = "ObsidianBullet" },
        external_link_icon = { char = "", hl_group = "ObsidianExtLinkIcon" },
        reference_text = { hl_group = "ObsidianRefText" },
        highlight_text = { hl_group = "ObsidianHighlightText" },
        tags = { hl_group = "ObsidianTag" },
        block_ids = { hl_group = "ObsidianBlockID" },
      },
      mappings = {
        ["gf"] = {
          action = function() return require("obsidian").util.gf_passthrough() end,
          opts = { noremap = false, expr = true, buffer = true },
        },
      },
    },
  },
}
