return {
  { "nvim-lua/plenary.nvim", lazy = true },
  { "tpope/vim-sleuth", event = "BufReadPost" },
  { "tpope/vim-eunuch", event = "CmdlineEnter" },
  { "tpope/vim-surround", event = "CursorHold" },
  { "guns/vim-sexp", event = "VeryLazy" },
  { "lewis6991/fileline.nvim" },
  { "andrewferrier/debugprint.nvim", version = "*", event = "VeryLazy", opts = true },
  { "phelipetls/jsonpath.nvim", ft = "json", opts = true },
  { "numToStr/Comment.nvim", event = "CursorHold", opts = true },
  { "windwp/nvim-autopairs", event = "BufModifiedSet", opts = true },
  {
    "echasnovski/mini.ai",
    version = false,
    event = "VeryLazy",
    dependencies = "nvim-treesitter/nvim-treesitter-textobjects",
  },
  {
    "mbbill/undotree",
    event = "VeryLazy",
    cond = vim.g.vscode == nil,
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
    version = false,
    lazy = true,
    opts = {
      labels = "asdfghjklzxcvbnmqwertyuiop",
      label = {
        uppercase = false,
        current = false,
        after = false,
        before = true,
        rainbow = { enabled = true, shade = 1 },
      },
      jump = { history = true, register = true, nohlsearch = true, autojump = true },
      modes = {
        search = { enabled = true },
        char = { jump_labels = true, label = { exclude = "hjkliadcor" }, multi_line = false },
      },
    },
  },
  { "MunifTanjim/nui.nvim", lazy = true },
  { "nvim-tree/nvim-web-devicons", lazy = true },
  { "folke/twilight.nvim", cmd = "Twilight" },
  { "stevearc/quicker.nvim", ft = "qf", opts = { borders = { vert = "│" } } },
  { "shortcuts/no-neck-pain.nvim", event = "VeryLazy", opts = true },
  {
    "j-hui/fidget.nvim",
    version = false,
    event = "VeryLazy",
    cond = vim.g.vscode == nil,
    opts = {
      notification = {
        override_vim_notify = true,
        window = {
          winblend = 0,
          max_width = 64,
          x_padding = 4,
          y_padding = 1,
          tabstop = 4,
          align = "top",
        },
        view = {
          reflow = "ellipsis",
          icon_separator = " │ ",
          group_separator = "────────",
        },
      },
    },
  },
  {
    "lukas-reineke/virt-column.nvim",
    event = "VeryLazy",
    cond = vim.g.vscode == nil,
    opts = { char = "│", highlight = "Comment" },
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    event = "VeryLazy",
    cond = vim.g.vscode == nil,
    opts = { indent = { char = "│" } },
  },
  {
    "norcalli/nvim-colorizer.lua",
    cmd = "ColorizerToggle",
    cond = vim.g.vscode == nil,
    opts = { ["*"] = { RRGGBBAA = true, rgb_fn = true, hsl_fn = true } },
  },
}
