return {
  { "nvim-lua/plenary.nvim", lazy = false, priority = 1000 },
  { "nvim-tree/nvim-web-devicons", lazy = true },
  { "tpope/vim-sleuth", lazy = true, event = { "BufAdd", "BufReadPost" } },
  { "tpope/vim-surround", lazy = true, event = { "BufAdd", "BufNewFile", "CursorMovedI" } },
  { "Pocco81/true-zen.nvim", event = "BufAdd" },
  { "mfussenegger/nvim-jdtls", ft = "java" },
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    opts = function()
      require("cmp").event:on("confirm_done", require("nvim-autopairs.completion.cmp").on_confirm_done())
      return { check_ts = true, ts_config = { lua = { "string" }, java = false } }
    end,
  },
  {
    "numToStr/Comment.nvim",
    event = { "BufAdd", "CursorMovedI" },
    opts = {
      opleader = { line = "gc", block = "gb" },
      mappings = { basic = true, extra = true },
      toggler = { line = "gcc", block = "gbc" },
    },
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    event = { "BufAdd", "BufNewFile" },
    main = "ibl",
    opts = { indent = { char = "│" } },
  },
  {
    "norcalli/nvim-colorizer.lua",
    cmd = "ColorizerToggle",
    keys = { { "<leader>ct", "<cmd>ColorizerToggle<CR>", desc = "[C]olorizer [T]oggle" } },
    opts = {
      ["*"] = {
        RGB = true,
        RRGGBB = true,
        RRGGBBAA = true,
        names = true,
        rgb_fn = false,
        hsl_fn = false,
        css = false,
        css_fn = false,
        mode = "background",
      },
    },
  },
}
