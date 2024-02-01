return {
  { "nvim-lua/plenary.nvim", lazy = false, priority = 900 },
  { "nvim-tree/nvim-web-devicons", lazy = true },
  { "tpope/vim-sleuth", event = { "BufReadPost" } },
  { "tpope/vim-surround", event = { "InsertEnter", "BufNewFile" } },
  { "Pocco81/true-zen.nvim", lazy = true, event = "BufAdd" },
  {
    "windwp/nvim-autopairs",
    lazy = true,
    event = "InsertEnter",
    opts = function()
      require("cmp").event:on("confirm_done", require("nvim-autopairs.completion.cmp").on_confirm_done())
      return { check_ts = true, ts_config = { lua = { "string" }, java = false } }
    end,
  },
  {
    "numToStr/Comment.nvim",
    lazy = true,
    event = "InsertEnter",
    opts = {
      opleader = { line = "gc", block = "gb" },
      mappings = { basic = true, extra = true },
      toggler = { line = "gcc", block = "gbc" },
    },
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    lazy = true,
    event = "InsertEnter",
    main = "ibl",
    opts = { indent = { char = "│" } },
  },
  {
    "folke/flash.nvim",
    lazy = true,
    event = "InsertEnter",
    opts = {
      label = { current = false, uppercase = false, after = false, before = true },
      modes = { char = { jump_labels = true, label = { exclude = "hjkliadc" } } },
    },
  },
}
