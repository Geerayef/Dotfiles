return {
  {
    "iamcco/markdown-preview.nvim",
    cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
    ft = "markdown",
    build = function() vim.fn["mkdp#util#install"]() end,
    keys = { "<leader>mp", ft = "markdown", "<cmd>MarkdownPreviewToggle<CR>", desc = "[m]arkdown [p]review" },
    config = function()
      -- default: 0
      vim.g.mkdp_auto_start = 0
      -- default: 1
      vim.g.mkdp_auto_close = 1
      -- default: 0
      vim.g.mkdp_refresh_slow = 1
      -- default: 0
      vim.g.mkdp_command_for_global = 0
      -- default: 0
      vim.g.mkdp_open_to_the_world = 0
      -- default: empty
      vim.g.mkdp_open_ip = ""
      -- default: ""
      vim.g.mkdp_browser = "firefox"
      -- default: 0
      vim.g.mkdp_echo_preview_url = 0
      -- default: empty
      vim.g.mkdp_browserfunc = ""
      vim.g.mkdp_preview_options = {
        ["mkit"] = {},
        ["katex"] = {},
        ["uml"] = {},
        ["maid"] = {},
        ["disable_sync_scroll"] = 0,
        ["sync_scroll_type"] = "middle",
        ["hide_yaml_meta"] = 0,
        ["sequence_diagrams"] = {},
        ["flowchart_diagrams"] = {},
        ["content_editable"] = false,
        ["disable_filename"] = 1,
        ["toc"] = {},
      }
      -- Absolute path or expand("~/markdown.css")
      vim.g.mkdp_markdown_css = ""
      -- Absolute path or expand("~/highlight.css")
      vim.g.mkdp_highlight_css = ""
      -- use a custom port to start server or empty for random
      vim.g.mkdp_port = ""
      vim.g.mkdp_page_title = "「${name} 」"
      -- use a custom location for images
      -- vim.g.mkdp_images_path = "/home/tibor/"
      vim.g.mkdp_filetypes = { "markdown" }
      vim.g.mkdp_theme = "dark"
      -- set vim.g.mkdp_auto_close = 0 if this is enabled
      -- default: 0
      vim.g.mkdp_combine_preview = 0
      -- auto refetch combine preview contents when change markdown buffer
      -- only when g:mkdp_combine_preview is 1
      vim.g.mkdp_combine_preview_auto_refresh = 1
    end,
  },
  {
    "epwalsh/obsidian.nvim",
    version = "*",
    ft = "markdown",
    opts = {
      workspaces = { { name = "notes", path = "~/notes" }, { name = "LGR", path = "~/notes/LGR" } },
      completion = { nvim_cmp = true, min_chars = 2 },
      preferred_link_style = "markdown",
      disable_frontmatter = false,
      picker = { name = "telescope.nvim", mappings = { new = "<C-x>", insert_link = "<C-l>" } },
      sort_by = "path",
      sort_reversed = false,
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
    },
  },
}
