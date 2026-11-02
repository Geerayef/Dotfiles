return {
  { "nvim-lua/plenary.nvim", lazy = true },
  { "tpope/vim-sleuth", event = "BufReadPost" },
  { "tpope/vim-eunuch", event = "CmdlineEnter" },
  { "tpope/vim-repeat", event = "CursorHold" },
  { "tpope/vim-surround", event = "CursorHold" },
  { "lewis6991/fileline.nvim" },
  { "windwp/nvim-autopairs", event = "BufModifiedSet", opts = true },
  { "numToStr/Comment.nvim", event = "CursorHold", opts = true },
  { "phelipetls/jsonpath.nvim", ft = "json", opts = true },
  {
    "andrewferrier/debugprint.nvim",
    version = "*",
    event = "VeryLazy",
    opts = { print_tag = "[DEBUG]" },
  },
  {
    "echasnovski/mini.ai",
    version = false,
    event = "VeryLazy",
    dependencies = "nvim-treesitter/nvim-treesitter-textobjects",
  },
  {
    "mbbill/undotree",
    event = "VeryLazy",
    config = function()
      vim.g.undotree_WindowLayout = 4
      vim.g.undotree_ShortIndicators = 1
      vim.g.undotree_SplitWidth = 64
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
  -- { "nvimtools/hydra.nvim" },
  { "MunifTanjim/nui.nvim", lazy = true },
  { "nvim-tree/nvim-web-devicons", lazy = true },
  { "folke/twilight.nvim", cmd = "Twilight" },
  { "stevearc/quicker.nvim", ft = "qf", opts = { borders = { vert = "│" } } },
  { "shortcuts/no-neck-pain.nvim", event = "VeryLazy", opts = true },
  { "lukas-reineke/virt-column.nvim", event = "VeryLazy", opts = { char = "│" } },
  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    event = "VeryLazy",
    opts = { indent = { char = "│" } },
  },
  {
    "norcalli/nvim-colorizer.lua",
    cmd = "ColorizerToggle",
    opts = { ["*"] = { RRGGBBAA = true, rgb_fn = true, hsl_fn = true } },
  },
}
