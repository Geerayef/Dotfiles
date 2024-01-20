return {
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    event = "BufReadPre",
    dependencies = { "nvim-treesitter/nvim-treesitter-textobjects" },
    config = function()
      local has_ts, ts = pcall(require, "nvim-treesitter.configs")
      if not has_ts then return end
      ts.setup({
        ensure_installed = { "bash", "fish", "lua", "luadoc", "c", "ocaml", "python", "rust", "yaml", "toml", "nu" },
        auto_install = true,
        highlight = {
          enable = true,
          disable = function(_, buf) return F.IsBigBuff(buf) end,
          additional_vim_regex_highlighting = false,
        },
        indent = { enable = true, disable = { "python" } },
        incremental_selection = {
          enable = true,
          -- keymaps = Keymaps.TS.incremental_selection,
          keymaps = {
            init_selection = "<C-space>",
            scope_incremental = "<C-space>",
            node_decremental = "<C-S><space>",
          },
        },
        textobjects = {
          select = {
            enable = true,
            lookahead = true,
            -- keymaps = Keymaps.TS.textobjects.select,
            {
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
            goto_next_start = { ["]f"] = "@function.outer", ["]]"] = "@class.outer" },
            goto_next_end = { ["]F"] = "@function.outer", ["]["] = "@class.outer" },
            goto_previous_start = { ["[f"] = "@function.outer", ["[["] = "@class.outer" },
            goto_previous_end = { ["[F"] = "@function.outer", ["[]"] = "@class.outer" },
          },
          swap = {
            enable = true,
            swap_next = { ["<leader>snp"] = "@parameter.inner" },
            swap_previous = { ["<leader>spp"] = "@parameter.inner" },
            -- swap_next = Keymaps.TS.swap_next,
            -- swap_previous = Keymaps.TS.swap_previous,
          },
          lsp_interop = {
            enable = true,
            border = "single",
            peek_definition_code = {
              ["<leader>pfd"] = "@function.outer",
              ["<leader>pcd"] = "@class.outer",
            },
          },
        },
      })
    end,
  },
}
