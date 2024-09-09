return {
  {
    "lewis6991/gitsigns.nvim",
    cond = function() return F.IsBufInRepo(0) end,
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
    "NeogitOrg/neogit",
    cmd = "Neogit",
    dependencies = { "lewis6991/gitsigns.nvim" },
    opts = {
      integrations = { telescope = true },
      telescope_sorter = function()
        return require("telescope").extensions.fzf.native_fzf_sorter()
      end,
      git_services = {
        ["github.com"] = "https://github.com/${owner}/${repository}/compare/${branch_name}?expand=1",
        ["gitlab.com"] = "https://gitlab.com/${owner}/${repository}/merge_requests/new?merge_request[source_branch]=${branch_name}",
        ["bitbucket.org"] = "https://bitbucket.org/${owner}/${repository}/pull-requests/new?source=${branch_name}&t=1",
      },
      use_per_project_settings = true,
    },
  },
}
