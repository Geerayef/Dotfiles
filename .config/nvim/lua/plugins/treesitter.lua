return {
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    event = "BufReadPre",
    dependencies = { "nvim-treesitter/nvim-treesitter-textobjects" },
    config = function()
      local ts = require("nvim-treesitter.configs")

      ts.setup({
        ensure_installed = { "bash", "fish", "lua", "luadoc", "c", "ocaml", "python", "rust", "yaml" },
        auto_install = true,
        highlight = {
          enable = true,
          disable = function(_, buf)
            local max_filesize = 100 * 1024
            local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
            return ok and stats and stats.size > max_filesize
          end,
          additional_vim_regex_highlighting = false,
        },
        indent = { enable = true, disable = { "python", "yaml" } },
        incremental_selection = {
          enable = true,
          keymaps = {
            init_selection = "<C-space>",
            -- node_incremental = false,
            -- scope_incremental = "<C-s>",
            -- node_decremental = "<C-S><space>",
          },
        },
        textobjects = {
          select = {
            enable = true,
            lookahead = true,
            keymaps = {
              -- You can use the capture groups defined in textobjects.scm
              ["aa"] = "@parameter.outer",
              ["ia"] = "@parameter.inner",
              ["af"] = "@function.outer",
              ["if"] = "@function.inner",
              ["ac"] = "@class.outer",
              ["ic"] = "@class.inner",
            },
          },
          move = {
            enable = true,
            set_jumps = true,
            goto_next_start = {
              ["]m"] = "@function.outer",
              ["]]"] = "@class.outer",
            },
            goto_next_end = {
              ["]M"] = "@function.outer",
              ["]["] = "@class.outer",
            },
            goto_previous_start = {
              ["[m"] = "@function.outer",
              ["[["] = "@class.outer",
            },
            goto_previous_end = {
              ["[M"] = "@function.outer",
              ["[]"] = "@class.outer",
            },
          },
          swap = {
            enable = true,
            swap_next = { ["<leader>snp"] = "@parameter.inner" },
            swap_previous = { ["<leader>spp"] = "@parameter.inner" },
          },
          lsp_interop = {
            enable = true,
            border = "single",
            floating_preview_opts = {},
            peek_definition_code = {
              ["<leader>pfd"] = "@function.outer",
              ["<leader>pcd"] = "@class.outer",
            },
          },
        },
      })
    end
  },
  -- {
  --   "nvim-treesitter/nvim-treesitter-textobjects",
  --   event = { "BufReadPost", "BufNewFile" },
  --   dependencies = { "nvim-treesitter/nvim-treesitter" },
  -- }
}
