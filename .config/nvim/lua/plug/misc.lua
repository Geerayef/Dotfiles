return {
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
  { "folke/twilight.nvim", lazy = true, cmd = "Twilight" },
  {
    "cdmill/focus.nvim",
    cmd = { "Focus", "Zen", "Narrow" },
    opts = {
      window = { backdrop = 1 },
      plugins = {
        gitsigns = { enabled = true },
        diagnostics = { enabled = true },
        twilight = { enabled = true },
      },
    },
  },
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
    "lukas-reineke/virt-column.nvim",
    event = "BufReadPost",
    opts = { char = "│", highlight = "Comment" },
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    event = "BufReadPost",
    opts = { indent = { char = "│" } },
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
    opts = {
      labels = "asdfghjklqwertyuiopzxcvbnm",
      label = {
        uppercase = false,
        current = false,
        after = false,
        before = true,
        rainbow = { enabled = true, shade = 1 },
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
          multi_line = false,
          jump = { autojump = true },
          label = { exclude = "hjkliadcor" },
        },
      },
    },
  },
  {
    "norcalli/nvim-colorizer.lua",
    cmd = "ColorizerToggle",
    opts = { ["*"] = { RRGGBBAA = true, rgb_fn = true, hsl_fn = true } },
  },
}
