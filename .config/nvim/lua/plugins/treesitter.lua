return {
  "nvim-treesitter/nvim-treesitter",
  version = false,
  build = ":TSUpdate",
  event = "VeryLazy",
  init = function(plugin)
    require("lazy.core.loader").add_to_rtp(plugin)
    require("nvim-treesitter.query_predicates")
  end,
  dependencies = { "nvim-treesitter/nvim-treesitter-textobjects" },
  opts = {
    ensure_installed = { "bash", "fish", "lua", "luadoc", "c", "ocaml", "python", "rust", "yaml", "toml", "nu" },
    auto_install = false,
    highlight = { enable = true, disable = function(_, buf) return F.IsBigBuff(buf) end },
    indent = { enable = true, disable = { "python" } },
    incremental_selection = {
      enable = true,
      -- keymaps = Keymaps.TS.incremental_selection,
      keymaps = { init_selection = "<C-space>", scope_incremental = "<C-space>", node_decremental = "<C-S><space>" },
    },
    textobjects = {
      select = {
        enable = true,
        lookahead = true,
        -- keymaps = Keymaps.TS.textobjects.select,
        keymaps = {
          ["fo"] = "@function.outer",
          ["fi"] = "@function.inner",
          ["co"] = "@class.outer",
          ["ci"] = "@class.inner",
        },
      },
      move = {
        enable = true,
        set_jumps = true,
        -- Keymaps.TS.
        goto_next_start = { ["[f"] = "@function.outer", ["]["] = "@class.outer" },
        goto_next_end = { ["]f"] = "@function.outer", ["]]"] = "@class.outer" },
        goto_previous_start = { ["[F"] = "@function.outer", ["[["] = "@class.outer" },
        goto_previous_end = { ["]F"] = "@function.outer", ["[]"] = "@class.outer" },
      },
      swap = {
        enable = true,
        swap_next = { ["<leader>snp"] = "@parameter.inner" },
        swap_previous = { ["<leader>spp"] = "@parameter.inner" },
        -- swap_next = Keymaps.TS.swap.next / previous
      },
      lsp_interop = {
        enable = true,
        border = "single",
        peek_definition_code = { ["<leader>pfd"] = "@function.outer", ["<leader>pcd"] = "@class.outer" },
      },
    },
  },
  config = function(_, opts) require("nvim-treesitter.configs").setup(opts) end,
}
