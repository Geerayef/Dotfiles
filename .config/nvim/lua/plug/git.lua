return {
  { "tpope/vim-fugitive", cmd = { "G", "Git" } },
  { "rbong/vim-flog", cmd = "Flog", dependencies = "tpope/vim-fugitive" },
  {
    "lewis6991/gitsigns.nvim",
    event = "VeryLazy",
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
    "NeogitOrg/neogit",
    cmd = "Neogit",
    opts = {
      graph_style = "unicode",
      kind = "replace",
      integrations = { fzf_lua = true, diffview = true },
      signs = {
        hunk = { S.Icons.ui.arrow_right, S.Icons.ui.arrow_down },
        item = { S.Icons.ui.arrow_right, S.Icons.ui.arrow_down },
        section = { S.Icons.ui.arrow_right, S.Icons.ui.arrow_down },
      },
    },
  },
  {
    "dlyongemallo/diffview.nvim",
    cmd = {
      "DiffviewOpen",
      "DiffviewClose",
      "DiffviewToggleFiles",
      "DiffviewFocusFiles",
      "DiffviewRefresh",
      "DiffviewFileHistory",
    },
    opts = {
      show_help_hints = true,
      view = {
        file_history = { layout = "diff1_plain" },
      },
      file_panel = {
        listing_style = "list",
        win_config = {
          type = "float",
          width = math.floor(vim.api.nvim_win_get_width(0) * 0.4),
          height = math.floor(vim.api.nvim_win_get_height(0) * 0.5),
          col = vim.o.columns * 0.25,
          row = vim.o.lines * 0.25,
        },
      },
      file_history_panel = {
        listing_style = "list",
        commit_subject_max_length = 80,
        win_config = {
          type = "float",
          width = math.floor(vim.api.nvim_win_get_width(0) * 0.8),
          height = math.floor(vim.api.nvim_win_get_height(0) * 0.5),
          col = vim.o.columns * 0.1,
          row = vim.o.lines * 0.25,
        },
      },
    },
  },
}
