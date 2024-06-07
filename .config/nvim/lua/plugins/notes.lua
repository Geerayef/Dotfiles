return {
  {
    "epwalsh/obsidian.nvim",
    version = "*",
    ft = "markdown",
    dependencies = {
      { "lukas-reineke/headlines.nvim", config = true },
      { "henriklovhaug/Preview.nvim", cmd = "Preview", config = true },
    },
    opts = {
      workspaces = { { name = "notes", path = "~/notes" }, { name = "LGR", path = "~/notes/LGR" } },
      daily_notes = { folder = "~/notes/daily", date_format = "%d-%m-%Y", alias_format = "%B %-d, %Y", template = nil },
      -- `substitutions`: map for custom variables. `key`: variable, `value`: function.
      templates = { folder = "~/notes/template", date_format = "%d-%m-%Y", time_format = "%H:%M", substitutions = {} },
      completion = { nvim_cmp = true, min_chars = 2 },
      preferred_link_style = "markdown",
      disable_frontmatter = true,
      sort_by = "path",
      sort_reversed = false,
      picker = { name = "telescope.nvim", mappings = { new = "<C-x>", insert_link = "<C-l>" } },
      ui = {
        enable = true,
        update_debounce = 200,
        checkboxes = {
          [" "] = { char = "󰄱", hl_group = "ObsidianTodo" },
          ["x"] = { char = "", hl_group = "ObsidianDone" },
          [">"] = { char = "", hl_group = "ObsidianRightArrow" },
          ["~"] = { char = "󰰱", hl_group = "ObsidianTilde" },
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
