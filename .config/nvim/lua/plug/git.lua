return {
  {
    "lewis6991/gitsigns.nvim",
    lazy = not F.IsBufInRepo(vim.api.nvim_get_current_buf()),
    dependencies = { "NeogitOrg/neogit", "sindrets/diffview.nvim" },
    cmd = "Gitsigns",
    opts = {
      signs = {
        add = { text = "│" },
        change = { text = "│" },
        delete = { text = "_" },
        topdelete = { text = "‾" },
        changedelete = { text = "~" },
        untracked = { text = "┆" },
      },
      signcolumn = true,
      numhl = false,
      linehl = false,
      word_diff = false,
      watch_gitdir = { follow_files = true },
      auto_attach = true,
      preview_config = {
        border = S.Border,
        style = "minimal",
        relative = "cursor",
        row = 0,
        col = 1,
      },
    },
  },
  {
    "sindrets/diffview.nvim",
    cmd = {
      "DiffviewOpen",
      "DiffviewClose",
      "DiffviewToggleFiles",
      "DiffviewFocusFiles",
      "DiffviewRefresh",
      "DiffviewFileHistory",
    },
    opts = { file_panel = { listing_style = "list" } },
  },
  {
    "NeogitOrg/neogit",
    cmd = "Neogit",
    opts = {
      graph_style = "kitty",
      kind = "floating",
      integrations = { fzf_lua = true },
      use_per_project_settings = true,
    },
  },
}
