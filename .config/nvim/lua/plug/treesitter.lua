return {
  {
    "nvim-treesitter/nvim-treesitter",
    branch = "main",
    build = ":TSUpdate",
    config = function()
      local ts = require("nvim-treesitter")
      local ensure = {
        "regex",
        "bash",
        "fish",
        "c",
        "cpp",
        "ocaml",
        "ocaml_interface",
        "menhir",
        "python",
        "rust",
        "go",
        "lua",
        "luadoc",
        "markdown",
        "markdown_inline",
        "vim",
        "vimdoc",
        "javascript",
        "typescript",
        "hyprlang",
        "toml",
        "yaml",
        "html",
      }
      local installed = ts.get_installed()
      ts.install(
        vim
          .iter(ensure)
          :filter(
            function(parser) return not vim.tbl_contains(installed, parser) end
          )
          :totable()
      )
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    branch = "main",
    keys = {
      {
        ";",
        function()
          require("nvim-treesitter-textobjects.repeatable_move").repeat_last_move()
        end,
        { "n", "x", "o" },
      },
      {
        ",",
        function()
          require("nvim-treesitter-textobjects.repeatable_move").repeat_last_move_opposite()
        end,
        { "n", "x", "o" },
      },
      {
        "af",
        function()
          require("nvim-treesitter-textobjects.select").select_textobject(
            "@function.outer",
            "textobjects"
          )
        end,
        { "x", "o" },
        desc = "around function",
      },
      {
        "if",
        function()
          require("nvim-treesitter-textobjects.select").select_textobject(
            "@function.inner",
            "textobjects"
          )
        end,
        { "x", "o" },
        desc = "in function",
      },
      {
        "<M-C-n>",
        function()
          require("nvim-treesitter-textobjects.swap").swap_next(
            "@parameter.inner"
          )
        end,
        desc = "Swap next parameter",
      },
      {
        "<M-C-p>",
        function()
          require("nvim-treesitter-textobjects.swap").swap_previous(
            "@parameter.inner"
          )
        end,
        desc = "Swap previous parameter",
      },
      {
        "<S-M-C-n>",
        function()
          require("nvim-treesitter-textobjects.swap").swap_next(
            "@function.outer"
          )
        end,
        desc = "Swap next function",
      },
      {
        "<S-M-C-p>",
        function()
          require("nvim-treesitter-textobjects.swap").swap_previous(
            "@function.outer"
          )
        end,
        desc = "Swap previous function",
      },
      {
        "]f",
        function()
          require("nvim-treesitter-textobjects.move").goto_next_start(
            "@function.outer",
            "textobjects"
          )
        end,
        { "n", "x", "o" },
        desc = "Go to next function start",
      },
      {
        "]F",
        function()
          require("nvim-treesitter-textobjects.move").goto_next_end(
            "@function.outer",
            "textobjects"
          )
        end,
        { "n", "x", "o" },
        desc = "Go to next function end",
      },
      {
        "[f",
        function()
          require("nvim-treesitter-textobjects.move").goto_previous_start(
            "@function.outer",
            "textobjects"
          )
        end,
        { "n", "x", "o" },
        desc = "Go to previous function start",
      },
      {
        "[F",
        function()
          require("nvim-treesitter-textobjects.move").goto_previous_end(
            "@function.outer",
            "textobjects"
          )
        end,
        { "n", "x", "o" },
        desc = "Go to previous function end",
      },
      {
        "][",
        function()
          require("nvim-treesitter-textobjects.move").goto_next_start(
            "@class.outer",
            "textobjects"
          )
        end,
        { "n", "x", "o" },
        desc = "Go to next class start",
      },
      {
        "]]",
        function()
          require("nvim-treesitter-textobjects.move").goto_next_end(
            "@class.outer",
            "textobjects"
          )
        end,
        { "n", "x", "o" },
        desc = "Go to next class end",
      },
      {
        "[[",
        function()
          require("nvim-treesitter-textobjects.move").goto_previous_start(
            "@class.outer",
            "textobjects"
          )
        end,
        { "n", "x", "o" },
        desc = "Go to previous class start",
      },
      {
        "[]",
        function()
          require("nvim-treesitter-textobjects.move").goto_previous_end(
            "@class.outer",
            "textobjects"
          )
        end,
        { "n", "x", "o" },
        desc = "Go to previous class end",
      },
      {
        "]z",
        function()
          require("nvim-treesitter-textobjects.move").goto_next_start(
            "@class.outer",
            "textobjects"
          )
        end,
        { "n", "x", "o" },
        desc = "Go to next fold",
      },
    },
    config = { select = { lookahead = true }, move = { set_jumps = true } },
  },
  {
    "nvim-treesitter/nvim-treesitter-context",
    event = "BufReadPost",
    config = {
      enable = true,
      multiwindow = false,
      max_lines = 4,
      min_window_height = 0,
      line_numbers = true,
      multiline_threshold = 20,
      trim_scope = "outer",
      mode = "cursor",
      separator = "─",
      zindex = 20,
      on_attach = nil,
    },
  },
}
