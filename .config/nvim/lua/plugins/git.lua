local border = require("util.objects").Border
return {
  {
    "lewis6991/gitsigns.nvim",
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
      on_attach = function(bufnr)
        local gs = require("gitsigns")
        local bo = { noremap = true, buffer = bufnr }
        local map = F.map
        map("n", "]h", function()
          if vim.wo.diff then return "]c" end
          vim.schedule(function() gs.next_hunk() end)
          return "<Ignore>"
        end, bo, "Next [h]unk")
        map("n", "[h", function()
          if vim.wo.diff then return "[c" end
          vim.schedule(function() gs.prev_hunk() end)
          return "<Ignore>"
        end, bo, "Previous [h]unk")
        map("v", "<leader>hs", function() gs.stage_hunk({ vim.fn.line("."), vim.fn.line("v") }) end, bo, "[h]unk [s]tage")
        map("v", "<leader>hr", function() gs.reset_hunk({ vim.fn.line("."), vim.fn.line("v") }) end, bo, "[h]unk [r]eset")
        map("n", "<leader>hb", function() gs.blame_line({ full = true }) end, bo, "[h]unk [b]lame line")
        map("n", "<leader>hD", function() gs.diffthis("~") end, bo, "[h]unk [D]iff this ~")
      end,
      preview_config = { border = border, style = "minimal", relative = "cursor", row = 0, col = 1 },
    },
  },
  {
    "NeogitOrg/neogit",
    cmd = "Neogit",
    opts = {
      integrations = { telescope = true },
      telescope_sorter = function() return require("telescope").extensions.fzf.native_fzf_sorter() end,
      git_services = {
        ["github.com"] = "https://github.com/${owner}/${repository}/compare/${branch_name}?expand=1",
        ["bitbucket.org"] = "https://bitbucket.org/${owner}/${repository}/pull-requests/new?source=${branch_name}&t=1",
        ["gitlab.com"] = "https://gitlab.com/${owner}/${repository}/merge_requests/new?merge_request[source_branch]=${branch_name}",
      },
      use_per_project_settings = true,
    },
  },
  -- {
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
