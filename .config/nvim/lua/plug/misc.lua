return {
  -- {
  --   name = "porphyrio",
  --   dir = "~/dev/porphyrio/",
  --   lazy = false,
  -- },
  { "nvim-lua/plenary.nvim", lazy = true },
  { "tpope/vim-surround", event = "CursorHold" },
  { "tpope/vim-sleuth", event = { "BufNewFile", "BufReadPost" } },
  { "phelipetls/jsonpath.nvim", ft = "json" },
  { "numToStr/Comment.nvim", event = "CursorHold", opts = true },
  { "windwp/nvim-autopairs", event = "BufModifiedSet", opts = true },
  {
    "echasnovski/mini.ai",
    version = false,
    event = "BufReadPost",
    dependencies = "nvim-treesitter/nvim-treesitter-textobjects",
  },
  { "stevearc/quicker.nvim", event = "VeryLazy", opts = true },
  { "nvim-tree/nvim-web-devicons", lazy = true },
  { "MunifTanjim/nui.nvim", lazy = true },
  { "folke/zen-mode.nvim", cmd = "ZenMode" },
  { "folke/twilight.nvim", lazy = true },
  {
    "j-hui/fidget.nvim",
    version = false,
    event = "VeryLazy",
    opts = {
      notification = {
        override_vim_notify = true,
        window = { max_width = 80, y_padding = 1 },
        view = {
          reflow = "ellipsis",
          icon_separator = " │ ",
          group_separator = "───",
        },
      },
    },
  },
  {
    "mbbill/undotree",
    event = "BufReadPost",
    config = function()
      vim.g.undotree_WindowLayout = 4
      vim.g.undotree_ShortIndicators = 1
      vim.g.undotree_SplitWidth = 40
      vim.g.undotree_SetFocusWhenToggle = 1
      vim.g.undotree_HelpLine = 0
    end,
  },
  {
    "folke/flash.nvim",
    lazy = true,
    opts = {
      labels = "asdfghjklqwertyuiopzxcvbnm",
      label = {
        current = false,
        after = false,
        before = true,
        rainbow = { enabled = true },
      },
      search = { mode = "fuzzy" },
      jump = {
        history = true,
        register = true,
        nohlsearch = true,
        autojump = true,
      },
      modes = {
        search = { enabled = true },
        char = {
          autohide = true,
          jump_labels = true,
          label = { exclude = "hjkliadcor" },
          multi_line = false,
        },
      },
      prompt = { prefix = { { " ⚡ ", "FlashPromptIcon" } } },
    },
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    event = "BufReadPost",
    main = "ibl",
    opts = { indent = { char = "│" } },
  },
  {
    "norcalli/nvim-colorizer.lua",
    cmd = "ColorizerToggle",
    opts = { ["*"] = { RRGGBBAA = true, rgb_fn = true, hsl_fn = true } },
  },
}
