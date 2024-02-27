local border = require("util.objects").Border
return {
  "nvim-treesitter/nvim-treesitter",
  version = false,
  build = ":TSUpdate",
  event = { "FileType" },
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
        "lua",
        "luadoc",
        "vim",
        "vimdoc",
        "bash",
        "fish",
        "c",
        "cpp",
        "ocaml",
        "python",
        "rust",
        "scheme",
        "commonlisp",
        "yaml",
        "toml",
        "typescript",
        "tsx",
        "javascript",
        "svelte",
        "html",
      },
      auto_install = false,
      highlight = {
        enable = true,
        disable = function(ft, buf) return F.IsBigBuff(buf) or ft == "latex" or vim.fn.win_gettype() == "command" end,
        additional_vim_regex_highlighting = { "markdown" },
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
