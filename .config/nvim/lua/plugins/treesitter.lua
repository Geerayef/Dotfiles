local border = require("util.objects").Border
return {
  "nvim-treesitter/nvim-treesitter",
  version = false,
  build = ":TSUpdate",
  event = { "BufRead", "BufWinEnter" },
  init = function(plugin)
    require("lazy.core.loader").add_to_rtp(plugin)
    require("nvim-treesitter.query_predicates")
  end,
  dependencies = { "nvim-treesitter/nvim-treesitter-textobjects" },
  opts = function()
    local ts = Key.TS
    local to = ts.textobjects
    local is = ts.incremental_selection
    return {
      ensure_installed = {
        "regex",
        "bash",
        "fish",
        "c",
        "cpp",
        "scheme",
        "commonlisp",
        "ocaml",
        "python",
        "rust",
        "hyprlang",
        "lua",
        "luadoc",
        "markdown",
        "markdown_inline",
        "html",
        "javascript",
        "typescript",
        "svelte",
        "yaml",
        "toml",
        "vim",
        "vimdoc",
      },
      auto_install = false,
      matchup = { enable = true },
      highlight = {
        enable = true,
        disable = function(ft, buf) return F.IsBigBuff(buf) or ft == "latex" or vim.fn.win_gettype() == "command" end,
      },
      indent = { enable = true, disable = { "python" } },
      incremental_selection = { enable = true, keymaps = is },
      textobjects = {
        select = { enable = true, lookahead = true, keymaps = to.select },
        move = {
          enable = true,
          set_jumps = true,
          goto_next_start = to.move.goto_next_start,
          goto_next_end = to.move.goto_next_end,
          goto_previous_start = to.move.goto_previous_start,
          goto_previous_end = to.move.goto_previous_end,
        },
        swap = { enable = true, swap_next = to.swap.next, swap_previous = to.swap.previous },
        lsp_interop = { enable = true, border = border, peek_definition_code = to.lsp_interop },
      },
    }
  end,
  config = function(_, opts) require("nvim-treesitter.configs").setup(opts) end,
}
