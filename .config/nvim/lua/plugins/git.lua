return {
  -- { "tpope/vim-fugitive", cmd = { "G", "Git" } },
  {
    "lewis6991/gitsigns.nvim",
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
      -- auto_attach = false,
      on_attach = function(bufnr)
        local gs = package.loaded.gitsigns
        local function map(mode, l, r, opts)
          opts = opts or {}
          opts.buffer = bufnr
          vim.keymap.set(mode, l, r, opts)
        end
        map("n", "]h", function()
          if vim.wo.diff then return "]c" end
          vim.schedule(function() gs.next_hunk() end)
          return "<Ignore>"
        end, { "Next [ ] ] [h]unk" })
        map("n", "[h", function()
          if vim.wo.diff then return "[c" end
          vim.schedule(function() gs.prev_hunk() end)
          return "<Ignore>"
        end, { "Previous [ [ ] [h]unk" })
        vim.keymap.set(
          "v",
          "<leader>hs",
          function() gs.stage_hunk({ vim.fn.line("."), vim.fn.line("v") }) end,
          { desc = "[h]unk [s]tage" }
        )
        vim.keymap.set(
          "v",
          "<leader>hr",
          function() gs.reset_hunk({ vim.fn.line("."), vim.fn.line("v") }) end,
          { desc = "[h]unk [r]eset" }
        )
        vim.keymap.set("n", "<leader>hb", function() gs.blame_line({ full = true }) end, { desc = "[h]unk [b]lame line" })
        vim.keymap.set("n", "<leader>hD", function() gs.diffthis("~") end, { desc = "[h]unk [D]iff this ~" })
      end,
      attach_to_untracked = false,
      current_line_blame = false,
      current_line_blame_opts = {
        virt_text = true,
        virt_text_pos = "eol",
        delay = 1000,
        ignore_whitespace = false,
        virt_text_priority = 100,
      },
      current_line_blame_formatter = "<author>, <author_time:%Y-%m-%d> - <summary>",
      sign_priority = 6,
      update_debounce = 100,
      status_formatter = nil,
      max_file_length = 40000,
      preview_config = { border = "rounded", style = "minimal", relative = "cursor", row = 0, col = 1 },
      yadm = { enable = false },
    },
  },
  {
    "NeogitOrg/neogit",
    cmd = "Neogit",
    opts = { use_default_keymaps = true, integrations = { telescope = nil, diffview = false, fzf_lua = false } },
  },
  -- Neogit Configuration:
  -- {
  -- disable_hint = false,
  -- disable_context_highlighting = false,
  -- disable_signs = false,
  -- disable_insert_on_commit = "auto",
  -- filewatcher = { interval = 1000, enabled = true },
  -- graph_style = "ascii", -- | "unicode"
  -- git_services = {
  --   ["github.com"] = "https://github.com/${owner}/${repository}/compare/${branch_name}?expand=1",
  --   ["bitbucket.org"] = "https://bitbucket.org/${owner}/${repository}/pull-requests/new?source=${branch_name}&t=1",
  --   ["gitlab.com"] = "https://gitlab.com/${owner}/${repository}/merge_requests/new?merge_request[source_branch]=${branch_name}",
  -- },
  -- telescope_sorter = function() return require("telescope").extensions.fzf.native_fzf_sorter() end,
  -- remember_settings = true,
  -- use_per_project_settings = true,
  -- ignored_settings = {
  --   "NeogitPushPopup--force-with-lease",
  --   "NeogitPushPopup--force",
  --   "NeogitPullPopup--rebase",
  --   "NeogitCommitPopup--allow-empty",
  --   "NeogitRevertPopup--no-edit",
  -- },
  -- highlight = { italic = true, bold = true, underline = true },
  -- use_default_keymaps = true,
  -- auto_refresh = true,
  -- sort_branches = "-committerdate",
  -- kind = "tab",
  -- disable_line_numbers = true,
  -- console_timeout = 2000,
  -- auto_show_console = true,
  -- status = { recent_commit_count = 10 },
  -- commit_editor = { kind = "auto" },
  -- commit_select_view = { kind = "tab" },
  -- commit_view = { kind = "vsplit", verify_commit = os.execute("which gpg") == 0 },
  -- log_view = { kind = "tab" },
  -- rebase_editor = { kind = "auto" },
  -- reflog_view = { kind = "tab" },
  -- merge_editor = { kind = "auto" },
  -- tag_editor = { kind = "auto" },
  -- preview_buffer = { kind = "split" },
  -- popup = { kind = "split" },
  -- signs = { hunk = { "", "" }, item = { ">", "v" }, section = { ">", "v" } },
  -- integrations = { telescope = nil, diffview = false, fzf_lua = false },
  -- sections = {
  --   sequencer = { folded = false, hidden = false },
  --   untracked = { folded = false, hidden = false },
  --   unstaged = { folded = false, hidden = false },
  --   staged = { folded = false, hidden = false },
  --   stashes = { folded = true, hidden = false },
  --   unpulled_upstream = { folded = true, hidden = false },
  --   unmerged_upstream = { folded = false, hidden = false },
  --   unpulled_pushRemote = { folded = true, hidden = false },
  --   unmerged_pushRemote = { folded = false, hidden = false },
  --   recent = { folded = true, hidden = false },
  --   rebase = { folded = true, hidden = false },
  -- },
  -- mappings = {
  --   commit_editor = { ["q"] = "Close", ["<c-c><c-c>"] = "Submit", ["<c-c><c-k>"] = "Abort", },
  --   rebase_editor = {
  --     ["p"] = "Pick",
  --     ["r"] = "Reword",
  --     ["e"] = "Edit",
  --     ["s"] = "Squash",
  --     ["f"] = "Fixup",
  --     ["x"] = "Execute",
  --     ["d"] = "Drop",
  --     ["b"] = "Break",
  --     ["q"] = "Close",
  --     ["<cr>"] = "OpenCommit",
  --     ["gk"] = "MoveUp",
  --     ["gj"] = "MoveDown",
  --     ["<c-c><c-c>"] = "Submit",
  --     ["<c-c><c-k>"] = "Abort",
  --   },
  --   finder = {
  --     ["<cr>"] = "Select",
  --     ["<c-c>"] = "Close",
  --     ["<esc>"] = "Close",
  --     ["<c-n>"] = "Next",
  --     ["<c-p>"] = "Previous",
  --     ["<down>"] = "Next",
  --     ["<up>"] = "Previous",
  --     ["<tab>"] = "MultiselectToggleNext",
  --     ["<s-tab>"] = "MultiselectTogglePrevious",
  --     ["<c-j>"] = "NOP",
  --   },
  --   popup = {
  --     ["?"] = "HelpPopup",
  --     ["A"] = "CherryPickPopup",
  --     ["D"] = "DiffPopup",
  --     ["M"] = "RemotePopup",
  --     ["P"] = "PushPopup",
  --     ["X"] = "ResetPopup",
  --     ["Z"] = "StashPopup",
  --     ["b"] = "BranchPopup",
  --     ["c"] = "CommitPopup",
  --     ["f"] = "FetchPopup",
  --     ["l"] = "LogPopup",
  --     ["m"] = "MergePopup",
  --     ["p"] = "PullPopup",
  --     ["r"] = "RebasePopup",
  --     ["v"] = "RevertPopup",
  --   },
  --   status = {
  --     ["q"] = "Close",
  --     ["I"] = "InitRepo",
  --     ["1"] = "Depth1",
  --     ["2"] = "Depth2",
  --     ["3"] = "Depth3",
  --     ["4"] = "Depth4",
  --     ["<tab>"] = "Toggle",
  --     ["x"] = "Discard",
  --     ["s"] = "Stage",
  --     ["S"] = "StageUnstaged",
  --     ["<c-s>"] = "StageAll",
  --     ["u"] = "Unstage",
  --     ["U"] = "UnstageStaged",
  --     ["$"] = "CommandHistory",
  --     ["#"] = "Console",
  --     ["Y"] = "YankSelected",
  --     ["<c-r>"] = "RefreshBuffer",
  --     ["<enter>"] = "GoToFile",
  --     ["<c-v>"] = "VSplitOpen",
  --     ["<c-x>"] = "SplitOpen",
  --     ["<c-t>"] = "TabOpen",
  --     ["{"] = "GoToPreviousHunkHeader",
  --     ["}"] = "GoToNextHunkHeader",
  --   },
  -- },
  -- },
}
