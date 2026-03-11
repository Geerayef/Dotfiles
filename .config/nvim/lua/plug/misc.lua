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
  {
    "j-hui/fidget.nvim",
    version = false,
    event = "VeryLazy",
    opts = {
      notification = {
        override_vim_notify = true,
        window = {
          border = S.BorderSimple,
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
          group_separator = "    ----    ",
        },
      },
    },
  },
  {
    "lukas-reineke/virt-column.nvim",
    event = "VeryLazy",
    opts = { char = "│", highlight = "Comment" },
  },
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
  {
    "rachartier/tiny-inline-diagnostic.nvim",
    opts = {
      -- Available: "modern", "classic", "minimal", "powerline", "ghost", "simple", "nonerdfont", "amongus"
      preset = "minimal",
      transparent_bg = false,
      transparent_cursorline = true,
      hi = {
        error = "DiagnosticError",
        warn = "DiagnosticWarn",
        info = "DiagnosticInfo",
        hint = "DiagnosticHint",
        arrow = "NonText",
        background = "CursorLine",
        mixing_color = "Normal",
      },
      options = {
        show_source = { enabled = true, if_many = true },
        show_code = true,
        use_icons_from_diagnostic = false,
        set_arrow_to_diag_color = false,
        throttle = 20,
        softwrap = 30,
        add_messages = {
          messages = true,
          display_count = true,
          use_max_severity = false,
          show_multiple_glyphs = false,
        },
        multilines = {
          enabled = true,
          always_show = false,
          trim_whitespaces = false,
          tabstop = 4,
          severity = nil,
        },
        show_all_diags_on_cursorline = false,
        show_diags_only_under_cursor = false,
        show_related = { enabled = true, max_count = 3 },
        overflow = { mode = "wrap", padding = 2 },
        break_line = { enabled = true, after = 64 },
        virt_texts = { priority = 2048 },
        severity = {
          vim.diagnostic.severity.ERROR,
          vim.diagnostic.severity.WARN,
          vim.diagnostic.severity.INFO,
          vim.diagnostic.severity.HINT,
        },
        overwrite_events = nil,
        override_open_float = true,
        experimental = { use_window_local_extmarks = true },
      },
    },
  },
}
