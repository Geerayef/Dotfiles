return {
  { "nvim-lua/plenary.nvim", lazy = true },
  { "nvim-tree/nvim-web-devicons", lazy = true },
  { "tpope/vim-sleuth", event = "BufEnter" },
  { "tpope/vim-surround", event = "BufReadPost" },
  { "mfussenegger/nvim-jdtls", ft = "java" },
  {
    "numToStr/Comment.nvim",
    event = "BufReadPost",
    opts = { extra = { above = "gcO", below = "gco" } },
  },
  {
    "norcalli/nvim-colorizer.lua",
    cmd = "ColorizerToggle",
    opts = { ["*"] = { RRGGBBAA = true } },
  },
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    opts = function()
      require("cmp").event:on(
        "confirm_done",
        require("nvim-autopairs.completion.cmp").on_confirm_done()
      )
      return {
        check_ts = true,
        ts_config = { lua = { "string" }, java = false },
      }
    end,
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    event = "BufReadPost",
    main = "ibl",
    cond = vim.g.disable_ibl,
    opts = { indent = { char = "│" } },
  },
  {
    "andymass/vim-matchup",
    event = "BufReadPost",
    config = function()
      vim.g.matchup_matchparen_offscreen = { method = "popup" }
      vim.g.matchup_surround_enabled = 1
      vim.g.matchup_matchparen_deferred = 1
      vim.g.matchup_override_vimtex = 1
    end,
  },
  {
    "williamboman/mason.nvim",
    cmd = "Mason",
    opts = {
      registries = { "github:mason-org/mason-registry" },
      ui = {
        icons = {
          package_installed = S.Icons.ui.box_check,
          package_pending = S.Icons.ui.arrow_r,
          package_uninstalled = S.Icons.ui.box_empty,
        },
        border = S.Border,
        width = 0.7,
        height = 0.5,
      },
    },
  },
}
